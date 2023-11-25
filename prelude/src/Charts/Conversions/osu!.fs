namespace Prelude.Charts.Conversions

open System
open System.IO
open System.Collections.Generic
open System.Linq
open Prelude
open Prelude.Charts.Formats.``osu!``
open Prelude.Charts

module Osu_To_Interlude =

    let private convert_hit_objects (objects: HitObject list) (keys: int) : TimeArray<NoteRow> =
        let output = List<TimeItem<NoteRow>>()
        let holding_until: Time option array = Array.zeroCreate keys
        let mutable last_row: TimeItem<NoteRow> = { Time = -Time.infinity; Data = [||] }

        let xToColumn (x: float) =
            x / 512.0 * float keys |> int |> min (keys - 1) |> max 0

        let finish_holds time =
            let mutable earliest_upcoming_release = Time.infinity

            let find_earliest_upcoming_release () =
                earliest_upcoming_release <- Time.infinity

                for h in holding_until do
                    match h with
                    | Some v -> earliest_upcoming_release <- min earliest_upcoming_release v
                    | None -> ()

            find_earliest_upcoming_release ()

            while earliest_upcoming_release < time do
                for k = 0 to keys - 1 do
                    if holding_until.[k] = Some earliest_upcoming_release then
                        assert(earliest_upcoming_release >= last_row.Time)
                        if earliest_upcoming_release > last_row.Time then
                            last_row <-
                                {
                                    Time = earliest_upcoming_release
                                    Data = Array.zeroCreate keys
                                }

                            output.Add last_row

                            for k = 0 to keys - 1 do
                                if holding_until.[k] <> None then
                                    last_row.Data.[k] <- NoteType.HOLDBODY

                        match last_row.Data.[k] with
                        | NoteType.NOTHING
                        | NoteType.HOLDBODY ->
                            last_row.Data.[k] <- NoteType.HOLDTAIL
                            holding_until.[k] <- None
                        | _ -> failwithf "impossible"

                find_earliest_upcoming_release ()

        let add_note column time =
            finish_holds time
            assert(time >= last_row.Time)
            if time > last_row.Time then
                last_row <-
                    {
                        Time = time
                        Data = Array.zeroCreate keys
                    }

                output.Add last_row

                for k = 0 to keys - 1 do
                    if holding_until.[k] <> None then
                        last_row.Data.[k] <- NoteType.HOLDBODY

            match last_row.Data.[column] with
            | NoteType.NOTHING -> last_row.Data.[column] <- NoteType.NORMAL
            | _ -> () //Logging.Debug(sprintf "Skipped note because it's stacked with %A" stack)

        let start_hold column time end_time =
            finish_holds time
            assert(time >= last_row.Time)
            if time > last_row.Time then
                last_row <-
                    {
                        Time = time
                        Data = Array.zeroCreate keys
                    }

                output.Add last_row

                for k = 0 to keys - 1 do
                    if holding_until.[k] <> None then
                        last_row.Data.[k] <- NoteType.HOLDBODY

            match last_row.Data.[column] with
            | NoteType.NOTHING ->
                last_row.Data.[column] <- NoteType.HOLDHEAD
                holding_until.[column] <- Some end_time
            | _ -> () //Logging.Debug(sprintf "Skipped hold because it's stacked with %A" stack)

        for object in objects |> List.sortBy (fun o -> o.Time) do
            match object with
            | HitCircle((x, _), time, _, _) -> add_note (xToColumn x) time
            | HoldNote((x, _), time, endTime, _, _) when endTime > time -> start_hold (xToColumn x) time endTime
            | HoldNote((x, _), time, endTime, _, _) -> add_note (xToColumn x) time
            | _ -> ()

        finish_holds Time.infinity
        output.ToArray()

    let rec private find_bpm_durations (points: TimingPoint list) (end_time: Time) : Dictionary<float32<ms/beat>, Time> =
        if List.isEmpty points then
            failwith "no bpm point"

        match List.head points with
        | TimingPoint.BPM(offset, mspb, _, _, _) ->
            let mutable current: float32<ms / beat> = mspb
            let mutable t: Time = offset
            let data = new Dictionary<float32<ms / beat>, Time>()

            for p in points do
                if (not (data.ContainsKey current)) then
                    data.Add(current, 0.0f<ms>)

                match p with
                | (TimingPoint.SV _) -> ()
                | (TimingPoint.BPM(offset, mspb, _, _, _)) ->
                    data.[current] <- data.[current] + offset - t
                    t <- offset
                    current <- mspb

            if (not (data.ContainsKey current)) then
                data.Add(current, 0.0f<ms>)

            data.[current] <- data.[current] + end_time - t
            data
        | _ -> find_bpm_durations (List.tail points) end_time

    let private convert_timing_points (points: TimingPoint list) (end_time: Time) : TimeArray<BPM> * TimeArray<float32> =
        let most_common_bpm =
            (find_bpm_durations points end_time)
                .OrderByDescending(fun p -> p.Value)
                .First()
                .Key

        let add_sv_value (offset, new_speed) sv =
            match sv with
            | [] -> [ { Time = offset; Data = new_speed } ]
            | { Time = time; Data = current_speed } :: s ->
                if current_speed = new_speed then
                    sv
                else
                    { Time = offset; Data = new_speed } :: sv

        let (bpm, sv, _) =
            let func
                ((bpm, sv, scroll): (TimeItem<BPM> list * TimeItem<float32> list * float32))
                (point: TimingPoint)
                : (TimeItem<BPM> list * TimeItem<float32> list * float32) =
                match point with
                | (TimingPoint.BPM(offset, msPerBeat, meter, _, _)) ->
                    {
                        Time = offset
                        Data = { Meter = meter; MsPerBeat = msPerBeat }
                    }
                    :: bpm,
                    add_sv_value (offset, (most_common_bpm / msPerBeat)) sv,
                    most_common_bpm / msPerBeat
                | (TimingPoint.SV(offset, value, _, _)) -> bpm, add_sv_value (offset, (value * scroll)) sv, scroll

            List.fold func ([], [], 1.0f) points

        bpm |> Array.ofList |> Array.rev, sv |> Array.ofList |> Array.rev

    let convert (b: Beatmap) (action: ConversionAction) : Chart =
        let keys = b.Difficulty.CircleSize |> int

        if b.General.Mode <> GameMode.Mania then skip_conversion "Beatmap is not osu!mania gamemode"
        if keys < 3 || keys > 10 then skip_conversion "Keymode not supported"
        if detect_rate_mod(b.Metadata.Version).IsSome then skip_conversion "Skipping rate modded beatmap"
        if b.Objects.Length < 20 then skip_conversion "Beatmap has very few or no notes"

        let path = Path.GetDirectoryName action.Source

        let rec find_background_file e =
            match e with
            | (Background(bg, _)) :: _ -> bg
            | _ :: es -> find_background_file es
            | [] -> ""

        let header =
            {
                Title = b.Metadata.Title.Trim()
                TitleNative =
                    let t = b.Metadata.TitleUnicode.Trim() in

                    if t.Length > 0 && t <> b.Metadata.Title.Trim() then
                        Some t
                    else
                        None
                Artist = b.Metadata.Artist.Trim()
                ArtistNative =
                    let t = b.Metadata.ArtistUnicode.Trim() in

                    if t.Length > 0 && t <> b.Metadata.Artist.Trim() then
                        Some t
                    else
                        None
                Creator = b.Metadata.Creator
                DiffName = b.Metadata.Version
                Subtitle = None
                Source = let t = b.Metadata.Source.Trim() in if t.Length > 0 then Some t else None
                Tags = b.Metadata.Tags

                PreviewTime = float32 b.General.PreviewTime * 1.0f<ms>
                BackgroundFile =
                    let r = find_background_file b.Events

                    if File.Exists(Path.Combine(path, r)) then
                        if action.Config.MoveAssets then
                            Relative r
                        else
                            Absolute(Path.Combine(path, r))
                    else
                        Missing
                AudioFile =
                    let r = b.General.AudioFilename

                    if File.Exists(Path.Combine(path, r)) then
                        if action.Config.MoveAssets then
                            Relative r
                        else
                            Absolute(Path.Combine(path, r))
                    else
                        Missing

                ChartSource = Osu(b.Metadata.BeatmapSetID, b.Metadata.BeatmapID)
            }

        let snaps = convert_hit_objects b.Objects keys

        let bpm, sv = convert_timing_points b.Timing (TimeArray.last snaps).Value.Time

        {
            Keys = keys
            Header = header
            Notes = snaps
            BPM = bpm
            SV = sv

            LoadedFromPath =
                Path.Combine(
                    path,
                    String.Join(
                        "_",
                        (b.Metadata.Title + " [" + b.Metadata.Version + "].yav")
                            .Split(Path.GetInvalidFileNameChars())
                    )
                )
        }