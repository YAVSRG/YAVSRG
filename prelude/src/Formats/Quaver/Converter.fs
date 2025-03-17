namespace Prelude.Formats.Quaver

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Formats

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
            | _ -> skip_conversion (sprintf "Stacked note at %f" time)

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
            | _ -> skip_conversion (sprintf "Stacked LN at %f" time)

        for object in objects |> List.sortBy (fun o -> o.StartTime) do
            if object.EndTime > object.StartTime then
                start_hold (object.Lane - 1) (Time.of_number object.StartTime) (Time.of_number object.EndTime)
            else
                add_note (object.Lane - 1) (Time.of_number object.StartTime)

        finish_holds Time.infinity
        output.ToArray()

    let convert (b: QuaverChart) (action: ConversionAction) : Result<ImportChart, SkippedConversion> =
        try
            let keys = b.Mode

            if b.HitObjects.Length < 20 then
                skip_conversion "Chart has less than 20 notes"

            let path = Path.GetDirectoryName action.Source

            let md5 =
                match QuaverChart.HashFromFile action.Source with
                | Ok s -> s
                | Error reason -> skip_conversion (sprintf "Failed to calculate MD5 of .qua file: %s" reason)

            let snaps = convert_hit_objects b.HitObjects keys

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
                        if b.BackgroundFile = "" then ImportAsset.Missing else

                        let absolute_background_path = Path.Combine(path, b.BackgroundFile)

                        match action.Config.AssetBehaviour with
                        | CopyAssetFiles -> ImportAsset.Copy absolute_background_path
                        | LinkAssetFiles -> ImportAsset.Link absolute_background_path
                    AudioFile =
                        if b.AudioFile = "" then ImportAsset.Missing else

                        let absolute_audio_path = Path.Combine(path, b.AudioFile)

                        match action.Config.AssetBehaviour with
                        | CopyAssetFiles -> ImportAsset.Copy absolute_audio_path
                        | LinkAssetFiles -> ImportAsset.Link absolute_audio_path

                    Origins =
                        {
                            Md5 = md5
                            MapSetId = b.MapSetId
                            MapId = b.MapId
                            FirstNoteOffset = snaps.[0].Time
                        }
                        |> ChartOrigin.Quaver
                        |> Set.singleton
                }

            if b.HasScratchKey then
                skip_conversion "HasScratchKey: true is not currently supported"

            if not b.BPMDoesNotAffectScrollVelocity then
                skip_conversion "BPMDoesNotAffectScrollVelocity: false is not currently supported"

            let bpm : TimeArray<BPM> =
                b.TimingPoints
                |> Seq.map (fun tp ->
                    {
                        Time = Time.of_number tp.StartTime
                        Data =
                            {
                                Meter = 4<beat>
                                MsPerBeat = 60000.0f<ms/minute> / (tp.Bpm * 1.0f<beat/minute>)
                            }
                    }
                ) |> Array.ofSeq

            let sv : TimeArray<SV> =
                seq {
                    if b.InitialScrollVelocity <> 1.0f then
                        yield { Time = -10000.0f<ms>; Data = b.InitialScrollVelocity }
                    yield!
                        b.SliderVelocities
                        |> Seq.map (fun sv ->
                            {
                                Time = Time.of_number sv.StartTime
                                Data = sv.Multiplier
                            }
                        )
                }
                |> Array.ofSeq

            Ok {
                Header = header
                LoadedFromPath = action.Source
                PackName = action.Config.PackName
                Chart = {
                    Keys = keys
                    Notes = snaps
                    BPM = bpm
                    SV = sv
                }
            }
        with
        | :? ConversionSkipException as skip_reason ->
            Error (action.Source, skip_reason.msg)
        | other_error ->
            Logging.Debug "Unexpected error converting %s: %O" action.Source other_error
            Error (action.Source, other_error.Message)