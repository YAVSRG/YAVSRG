namespace Prelude.Data.Library

open System.IO
open System.IO.Compression
open Prelude
open Prelude.Charts
open Prelude.Charts.Formats.osu

module Interlude_To_Osu =

    let private notes_to_hitobjects (notes: TimeArray<NoteRow>) (keys: int) =
        let rec ln_lookahead k (snaps: TimeItem<NoteRow> list) =
            match snaps with
            | { Time = offset; Data = nr } :: ss ->
                if nr.[k] = NoteType.HOLDTAIL then
                    offset
                else
                    ln_lookahead k ss
            | [] -> failwith "hold note has no end"

        let rec convert (snaps: TimeItem<NoteRow> list) =
            seq {
                match snaps with
                | { Time = offset; Data = nr } :: ss ->
                    for k = 0 to keys - 1 do
                        if nr.[k] = NoteType.NORMAL then
                            yield HitObject.CreateManiaNote(keys, k, offset)
                        elif nr.[k] = NoteType.HOLDHEAD then
                            yield HitObject.CreateManiaHold(keys, k, offset, ln_lookahead k ss)

                    yield! convert ss
                | [] -> ()
            }

        convert (notes |> Array.toList) |> List.ofSeq

    let private convert_timing_points
        (bpm: TimeArray<BPM>)
        (sv: TimeArray<float32>)
        (most_common_mspb: float32<ms / beat>)
        =

        let corrective_sv offset mult =
            if sv.Length = 0 then
                None
            else
                let index = TimeArray.find_left offset sv

                if index < 0 then
                    None
                else
                    let { Time = time; Data = value } = sv.[index]

                    if time = offset then
                        None
                    else
                        Some(TimingPoint.CreateSV(time, mult * value))

        let svs time1 time2 mult =
            seq {
                match corrective_sv time1 mult with
                | None -> ()
                | Some x -> yield x

                for { Time = offset; Data = value } in TimeArray.between time1 time2 sv do
                    yield TimingPoint.CreateSV(offset, mult * value)
            }

        let tps =
            seq {
                let mutable bs = bpm |> List.ofArray

                if List.isEmpty bs then
                    ()
                else

                    yield! svs (-Time.infinity) (List.head bs).Time 1.0f

                    while not (List.isEmpty bs) do
                        match bs with
                        | {
                              Time = offset
                              Data = { Meter = meter; MsPerBeat = mspb }
                          } :: { Time = offset2 } :: rs ->
                            yield TimingPoint.CreateBPM(offset, mspb, meter)
                            yield! svs offset offset2 (most_common_mspb / mspb)
                            bs <- List.tail bs
                        | {
                              Time = offset
                              Data = {
                                         Meter = meter
                                         MsPerBeat = mspb
                                     }
                          } :: [] ->
                            yield TimingPoint.CreateBPM(offset, mspb, meter)
                            yield! svs offset Time.infinity (most_common_mspb / mspb)
                            bs <- List.tail bs
                        | [] -> failwith "impossible by loop condition"
            }

        tps |> List.ofSeq

    let convert (chart: Chart) (chart_meta: ChartMeta) : Beatmap =
        let general : General =
            {
                AudioFilename =
                    match chart_meta.Audio with
                    | AssetPath.Absolute s -> Path.GetFileName s
                    | AssetPath.Hash _
                    | AssetPath.Missing -> "audio.mp3"
                AudioLeadIn = 0
                PreviewTime = int chart_meta.PreviewTime
                Countdown = Countdown.None
                SampleSet = SampleSet.Soft
                StackLeniency = 0.7
                Mode = Gamemode.OSU_MANIA
                LetterboxInBreaks = false
                UseSkinSprites = false
                OverlayPosition = OverlayPosition.NoChange
                SkinPreference = ""
                EpilepsyWarning = false
                CountdownOffset = 0
                SpecialStyle = false
                WidescreenStoryboard = false
                SamplesMatchPlaybackRate = false
            }

        let editor = Editor.Default

        let meta =
            { Metadata.Default with
                Title = chart_meta.Title
                TitleUnicode = Option.defaultValue chart_meta.Title chart_meta.TitleNative
                Artist = chart_meta.Artist
                ArtistUnicode = Option.defaultValue chart_meta.Artist chart_meta.ArtistNative
                Creator = chart_meta.Creator
                Version = chart_meta.DifficultyName
            }

        let diff =
            {
                CircleSize = float chart.Keys
                OverallDifficulty = 8.0
                HPDrainRate = 8.0
                ApproachRate = 5.0
                SliderMultiplier = 1.4
                SliderTickRate = 1.0
            }

        {
            General = general
            Editor = editor
            Metadata = meta
            Difficulty = diff
            Events =
                [
                    Background(
                        ( match chart_meta.Background with
                          | AssetPath.Absolute s -> Path.GetFileName s
                          | AssetPath.Hash _
                          | AssetPath.Missing -> "bg.png"),
                        0, 0
                    )
                ]
            Objects = notes_to_hitobjects chart.Notes chart.Keys
            Timing = convert_timing_points chart.BPM chart.SV (Chart.find_most_common_bpm chart)
        }

module Exports =

    let create_osz (chart: Chart) (chart_meta: ChartMeta) (od: float32) (export_folder: string) : Result<Beatmap * string, exn> =
        try
            let beatmap = Interlude_To_Osu.convert chart chart_meta

            let beatmap =
                { beatmap with
                     Difficulty = 
                        { beatmap.Difficulty with
                            OverallDifficulty = float od
                        }
                }

            let file_name = beatmap.Filename
            let archive_file_name = beatmap.Filename.Replace(".osu", ".osz")
            let archive_path = Path.Combine(export_folder, archive_file_name)

            use fs = File.Open(archive_path, FileMode.Create)
            use archive = new ZipArchive(fs, ZipArchiveMode.Create, false)

            do
                let osu_file_entry = archive.CreateEntry(file_name)
                use osu_file_stream = osu_file_entry.Open()
                beatmap.ToStream (osu_file_stream, false)

            do
                match chart_meta.Background.Path with
                | Some bg_path ->
                    use fs = File.Open(bg_path, FileMode.Open)
                    let bg_file_entry = archive.CreateEntry(beatmap.Events |> Seq.pick (function Background(bg, _, _) -> Some bg | _ -> None))
                    use bg_file_stream = bg_file_entry.Open()
                    fs.CopyTo(bg_file_stream)
                | None -> ()

            do
                match chart_meta.Audio.Path with
                | Some audio_path ->
                    use fs =
                        File.Open(audio_path, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite)

                    let audio_file_entry = archive.CreateEntry(beatmap.General.AudioFilename)
                    use audio_file_stream = audio_file_entry.Open()
                    fs.CopyTo(audio_file_stream)
                | None -> ()
            Ok (beatmap, archive_file_name)
        with err ->
            Error err
