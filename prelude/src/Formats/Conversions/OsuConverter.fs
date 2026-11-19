namespace Prelude.Formats.Conversions

open System
open System.IO
open System.Collections.Generic
open System.Linq
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Formats.Osu

module OsuConverter =

    let convert_hit_objects (objects: HitObject list) (keys: int) : TimeArray<NoteRow> =

        let inline x_to_column (x: float) : int =
            x / 512.0 * float keys |> int |> min(keys - 1) |> max 0

        let holding_until: Time option array = Array.zeroCreate keys
        let output = List<TimeItem<NoteRow>>()
        let mutable last_row: TimeItem<NoteRow> = { Time = -Time.infinity; Data = [||] }

        let inline find_earliest_upcoming_release () : Time =
            let mutable earliest_upcoming_release = Time.infinity

            for h in holding_until do
                match h with
                | Some v -> earliest_upcoming_release <- min earliest_upcoming_release v
                | None -> ()

            earliest_upcoming_release

        let finish_holds (time: Time) =
            let mutable earliest_upcoming_release = find_earliest_upcoming_release()

            while earliest_upcoming_release < time do
                for k = 0 to keys - 1 do
                    if holding_until.[k] = Some earliest_upcoming_release then
                        assert (earliest_upcoming_release >= last_row.Time)

                        if earliest_upcoming_release > last_row.Time then
                            last_row <- { Time = earliest_upcoming_release; Data = Array.zeroCreate keys }

                            output.Add(last_row)

                            for k = 0 to keys - 1 do
                                if holding_until.[k] <> None then
                                    last_row.Data.[k] <- NoteType.HOLDBODY

                        match last_row.Data.[k] with
                        | NoteType.NOTHING
                        | NoteType.HOLDBODY ->
                            last_row.Data.[k] <- NoteType.HOLDTAIL
                            holding_until.[k] <- None
                        | _ -> failwithf "impossible"

                earliest_upcoming_release <- find_earliest_upcoming_release()

        let add_note (column: int) (time: Time) =
            finish_holds(time)
            assert (time >= last_row.Time)

            if time > last_row.Time then
                last_row <- { Time = time; Data = Array.zeroCreate keys }

                output.Add(last_row)

                for k = 0 to keys - 1 do
                    if holding_until.[k] <> None then
                        last_row.Data.[k] <- NoteType.HOLDBODY

            match last_row.Data.[column] with
            | NoteType.NOTHING -> last_row.Data.[column] <- NoteType.NORMAL
            | NoteType.NORMAL
            | NoteType.HOLDHEAD -> Logging.Debug "Fixing stacked note at %f, column %i" time (column + 1)
            | other ->
                skip_conversion(sprintf "Stacked note at %f, column %i, coincides with %A" time (column + 1) other)

        let start_hold (column: int) (time: Time) (end_time: Time) =
            finish_holds(time)
            assert (time >= last_row.Time)
            assert (end_time > time)

            if time > last_row.Time then
                last_row <- { Time = time; Data = Array.zeroCreate keys }

                output.Add(last_row)

                for k = 0 to keys - 1 do
                    if holding_until.[k] <> None then
                        last_row.Data.[k] <- NoteType.HOLDBODY

            match last_row.Data.[column] with
            | NoteType.NOTHING ->
                last_row.Data.[column] <- NoteType.HOLDHEAD
                holding_until.[column] <- Some end_time
            | NoteType.NORMAL ->
                Logging.Debug "Fixing stacked note + LN head at %f, column %i" time (column + 1)
                last_row.Data.[column] <- NoteType.HOLDHEAD
                holding_until.[column] <- Some end_time
            | other ->
                skip_conversion(sprintf "Stacked LN at %f, column %i, head coincides with %A" time (column + 1) other)

        // The osu! client sorts all hitobjects and timing points by timestamp for when users have put them in the wrong order via notepad
        for object in objects |> List.sortBy _.Time do
            match object with
            | HitCircle x -> add_note (x_to_column x.X) (Time.of_number x.Time)
            | Hold x when x.EndTime > x.Time ->
                start_hold (x_to_column x.X) (Time.of_number x.Time) (Time.of_number x.EndTime)
            | Hold x -> add_note (x_to_column x.X) (Time.of_number x.Time)
            | _ -> ()

        finish_holds(Time.infinity)
        output.ToArray()

    let private find_most_common_beat_length (points: TimingPoint list) (end_time: Time) : float32<ms / beat> =

        let data = Dictionary<float32<ms / beat>, Time>()

        let uninherited =
            points
            |> Seq.choose(
                function
                | Uninherited b -> Some b
                | _ -> None
            )
            |> List.ofSeq

        match uninherited with
        | [] -> failwith "unreachable due to the check before this function is called"
        | x :: xs ->
            let mutable current: float32<ms / beat> = abs(float32 x.MsPerBeat) * 1.0f<ms / beat>
            let mutable time = Time.of_number x.Time

            for b in xs do
                if (not(data.ContainsKey(current))) then
                    data.Add(current, 0.0f<ms>)

                data.[current] <- data.[current] + Time.of_number b.Time - time
                time <- Time.of_number b.Time
                current <- abs(float32 b.MsPerBeat) * 1.0f<ms / beat>

            if (not(data.ContainsKey(current))) then
                data.Add(current, 0.0f<ms>)

            data.[current] <- data.[current] + max (end_time - time) 0.0f<ms>

        data.OrderByDescending(_.Value).First().Key

    let convert_timing_points (points: TimingPoint list) (end_time: Time) : TimeArray<BPM> * TimeArray<float32> =

        // The osu! client sorts all hitobjects and timing points by timestamp for when users have put them in the wrong order via notepad
        // IMPORTANT: If multiple timing points are stacked on the same timestamp,
        //   osu! will act UNPREDICTABLY and put them in an arbitrary order since it does not use a stable sort
        // I have ruled to use a stable sort (`List.sortBy`)
        //   This means stacked timing points will stay in the order they are in the .osu file after being sorted
        // This gives a defined behaviour to my parse and conversion but anyone writing their own should be aware that it's UB
        let points = points |> List.sortBy _.Time

        match List.tryHead points with
        | Some(Inherited _) -> skip_conversion "First timing point must be uninherited"
        | None -> skip_conversion "This beatmap has no timing points"
        | _ -> ()

        let most_common_mspb = find_most_common_beat_length points end_time
        let sv = ResizeArray<TimeItem<float32>>()
        let bpm = ResizeArray<TimeItem<BPM>>()
        let mutable current_bpm_mult = 1.0f

        for point in points do
            match point with
            | Uninherited uninherited ->
                let beat_length = abs(float32 uninherited.MsPerBeat) * 1.0f<ms / beat>

                bpm.Add(
                    {
                        Time = Time.of_number uninherited.Time
                        Data = { Meter = uninherited.Meter * 1<beat>; MsPerBeat = beat_length }
                    }
                )

                current_bpm_mult <- most_common_mspb / beat_length
                sv.Add({ Time = Time.of_number uninherited.Time; Data = current_bpm_mult })

            | Inherited inherited ->
                sv.Add({ Time = Time.of_number inherited.Time; Data = current_bpm_mult * float32 inherited.Multiplier })

        remove_stacked_points(bpm.ToArray()), cleanup_sv(sv.ToArray())

    let convert (b: Beatmap) (ctx: ConversionContext) : Result<ImportChart, SkippedConversion> =
        try
            let keys = int b.Difficulty.CircleSize

            if b.General.Mode <> Gamemode.OSU_MANIA then
                skip_conversion "Beatmap is not osu!mania gamemode"

            if keys < 3 || keys > 10 then
                skip_conversion "Keymode not supported"

            if b.Objects.Length < 20 then
                skip_conversion "Beatmap has less than 20 notes"

            let path = Path.GetDirectoryName(ctx.OriginalPath)

            let rec find_background_file (events: StoryboardEvent list) =
                match events with
                | Background(bg, _, _) :: _ -> bg
                | _ :: es -> find_background_file es
                | [] -> ""

            let md5 =
                match Beatmap.HashFromFile(ctx.OriginalPath) with
                | Ok s -> s
                | Error reason -> skip_conversion(sprintf "Failed to calculate MD5 of .osu file: %s" reason)

            let notes = convert_hit_objects b.Objects keys
            let bpm, sv = convert_timing_points b.Timing (TimeArray.last notes).Value.Time

            let header =
                {
                    Title = b.Metadata.Title.Trim()
                    TitleNative =
                        let t = b.Metadata.TitleUnicode.Trim() in

                        if t.Length > 0 && t <> b.Metadata.Title.Trim() then Some t else None
                    Artist = b.Metadata.Artist.Trim()
                    ArtistNative =
                        let t = b.Metadata.ArtistUnicode.Trim() in

                        if t.Length > 0 && t <> b.Metadata.Artist.Trim() then Some t else None
                    Creator = b.Metadata.Creator
                    DiffName = b.Metadata.Version
                    Subtitle = None
                    Source = let t = b.Metadata.Source.Trim() in if t.Length > 0 then Some t else None
                    Tags = b.Metadata.Tags

                    PreviewTime = float32 b.General.PreviewTime * 1.0f<ms>
                    BackgroundFile =
                        let background_file = find_background_file b.Events

                        if background_file = "" then
                            ImportAsset.Missing
                        else

                        let absolute_background_path = Path.Combine(path, background_file)

                        match ctx.Options.AssetMode with
                        | Copy -> ImportAsset.Copy(absolute_background_path)
                        | Link -> ImportAsset.Link(absolute_background_path)
                    AudioFile =
                        let audio_file = b.General.AudioFilename

                        if audio_file = "" then
                            ImportAsset.Missing
                        else

                        let absolute_audio_path = Path.Combine(path, audio_file)

                        match ctx.Options.AssetMode with
                        | Copy -> ImportAsset.Copy(absolute_audio_path)
                        | Link -> ImportAsset.Link(absolute_audio_path)

                    Origins =
                        {
                            Md5 = md5
                            BeatmapSetId = b.Metadata.BeatmapSetID
                            BeatmapId = b.Metadata.BeatmapID
                            SourceRate = 1.0f<rate>
                            FirstNoteOffset = notes.[0].Time
                            SourceOD = Math.Clamp(MathF.Round(float32 b.Difficulty.OverallDifficulty, 1), 0.0f, 10.0f)
                        }
                        |> ChartOrigin.Osu
                        |> Set.singleton
                }

            Ok
                {
                    Header = header
                    LoadedFromPath = ctx.OriginalPath
                    PackName = ctx.Options.PackName
                    Chart =
                        {
                            Keys = keys
                            Notes = notes
                            BPM = bpm
                            SV = sv
                        }
                }
        with
        | :? ConversionSkipException as skip_reason ->
            Error { OriginalPath = ctx.OriginalPath; Reason = skip_reason.Reason }
        | other_error ->
            Logging.Debug "Unexpected error converting %s: %O" ctx.OriginalPath other_error
            Error { OriginalPath = ctx.OriginalPath; Reason = other_error.Message }
