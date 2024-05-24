namespace Prelude.Charts.Conversions

open System
open System.IO
open System.Collections.Generic
open System.Linq
open Prelude
open Prelude.Charts.Formats.Quaver
open Prelude.Charts

module Quaver_To_Interlude =

    let private convert_hit_objects (objects: QuaverHitObject list) (keys: int) : TimeArray<NoteRow> =
        let output = List<TimeItem<NoteRow>>()
        let holding_until: Time option array = Array.zeroCreate keys
        let mutable last_row: TimeItem<NoteRow> = { Time = -Time.infinity; Data = [||] }

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
                        assert (earliest_upcoming_release >= last_row.Time)

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
            assert (time >= last_row.Time)

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
            | _ -> ()

        let start_hold column time end_time =
            finish_holds time
            assert (time >= last_row.Time)

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
            | _ -> ()

        for object in objects |> List.sortBy (fun o -> o.StartTime) do
            if object.EndTime > object.StartTime then
                start_hold (object.Lane - 1) (Time.of_number object.StartTime) (Time.of_number object.EndTime)
            else
                add_note (object.Lane - 1) (Time.of_number object.StartTime)

        finish_holds Time.infinity
        output.ToArray()

    let rec private find_bpm_durations
        (points: QuaverTimingPoint list)
        (end_time: Time)
        : Dictionary<float32<ms / beat>, Time> =
        if List.isEmpty points then
            failwith "no bpm point"

        let point = List.head points

        let mutable current: float32<ms / beat> = 60000.0f<ms/minute> / (point.Bpm * 1.0f<beat/minute>)
        let mutable t: Time = Time.of_number point.StartTime
        let data = new Dictionary<float32<ms / beat>, Time>()

        for p in points do
            if (not (data.ContainsKey current)) then
                data.Add(current, 0.0f<ms>)

            data.[current] <- data.[current] + Time.of_number p.StartTime - t
            t <- Time.of_number p.StartTime
            current <- 60000.0f<ms/minute> / (p.Bpm * 1.0f<beat/minute>)

        if (not (data.ContainsKey current)) then
            data.Add(current, 0.0f<ms>)

        data.[current] <- data.[current] + end_time - t
        data

    //let private convert_timing_points
    //    (points: TimingPoint list)
    //    (end_time: Time)
    //    : TimeArray<BPM> * TimeArray<float32> =
    //    let most_common_bpm =
    //        (find_bpm_durations points end_time)
    //            .OrderByDescending(fun p -> p.Value)
    //            .First()
    //            .Key

    //    let add_sv_value (offset, new_speed) sv =
    //        match sv with
    //        | [] -> [ { Time = offset; Data = new_speed } ]
    //        | { Time = time; Data = current_speed } :: s ->
    //            if current_speed = new_speed then
    //                sv
    //            else
    //                { Time = offset; Data = new_speed } :: sv

    //    let (bpm, sv, _) =
    //        let func
    //            ((bpm, sv, scroll): (TimeItem<BPM> list * TimeItem<float32> list * float32))
    //            (point: TimingPoint)
    //            : (TimeItem<BPM> list * TimeItem<float32> list * float32) =
    //            match point with
    //            | (TimingPoint.BPM(offset, msPerBeat, meter, _, _)) ->
    //                {
    //                    Time = offset
    //                    Data = { Meter = meter; MsPerBeat = msPerBeat }
    //                }
    //                :: bpm,
    //                add_sv_value (offset, (most_common_bpm / msPerBeat)) sv,
    //                most_common_bpm / msPerBeat
    //            | (TimingPoint.SV(offset, value, _, _)) -> bpm, add_sv_value (offset, (value * scroll)) sv, scroll

    //        List.fold func ([], [], 1.0f) points

    //    bpm |> Array.ofList |> Array.rev, sv |> Array.ofList |> Array.rev

    let convert (b: QuaverChart) (action: ConversionAction) : Chart =
        let keys = b.Mode

        if b.HitObjects.Length < 20 then
            skip_conversion "Beatmap has very few or no notes"

        let path = Path.GetDirectoryName action.Source

        let header =
            {
                Title = b.Title
                TitleNative = None
                Artist = b.Artist
                ArtistNative = None
                Creator = b.Creator
                DiffName = b.DifficultyName
                Subtitle = None
                Source = if b.Source.Length > 0 then Some b.Source else None
                Tags = b.Tags

                PreviewTime = Time.of_number b.SongPreviewTime
                BackgroundFile =
                    let requested_file = b.BackgroundFile

                    if File.Exists(Path.Combine(path, requested_file)) then
                        if action.Config.MoveAssets then
                            Relative requested_file
                        else
                            Absolute(Path.Combine(path, requested_file))
                    else
                        Missing
                AudioFile =
                    let requested_file = b.AudioFile

                    if File.Exists(Path.Combine(path, requested_file)) then
                        if action.Config.MoveAssets then
                            Relative requested_file
                        else
                            Absolute(Path.Combine(path, requested_file))
                    else
                        Missing

                ChartSource = Quaver(b.MapSetId, b.MapId)
            }

        let snaps = convert_hit_objects b.HitObjects keys

        let bpm, sv = failwith "nyi"//convert_timing_points b.Timing (TimeArray.last snaps).Value.Time

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
                        (b.Title + " [" + b.DifficultyName + "].yav")
                            .Split(Path.GetInvalidFileNameChars())
                    )
                )
        }
