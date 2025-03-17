namespace Prelude.Data.Library

open System.IO
open System.IO.Compression
open Prelude
open Prelude.Charts
open Prelude.Formats.Osu

type OsuExportOptions =
    {
        OD: float
        HP: float
    }
    static member Default = { OD = 8.0; HP = 8.0 }

module OsuExport =

    let notes_to_hitobjects (notes: TimeArray<NoteRow>) (keys: int) : HitObject list =
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

    let convert_timing_points
        (bpm: TimeArray<BPM>)
        (sv: TimeArray<float32>)
        (most_common_mspb: float32<ms / beat>)
        : TimingPoint list =

        let result = ResizeArray<TimingPoint>()

        let mutable bpm_i = 0
        let mutable sv_i = 0

        let mutable bpm_multiplier = 1.0f
        let mutable intended_scroll_multiplier = 1.0f

        while bpm_i < bpm.Length && sv_i < sv.Length do

            let next_bpm = bpm.[bpm_i]
            let next_sv = sv.[sv_i]

            if next_bpm.Time <= next_sv.Time then
                bpm_multiplier <- most_common_mspb / next_bpm.Data.MsPerBeat
                bpm_i <- bpm_i + 1
                result.Add(TimingPoint.CreateBPM(next_bpm.Time, next_bpm.Data.MsPerBeat, next_bpm.Data.Meter))
                result.Add(TimingPoint.CreateSV(next_bpm.Time, intended_scroll_multiplier / bpm_multiplier))

            else
                sv_i <- sv_i + 1
                intended_scroll_multiplier <- next_sv.Data
                result.Add(TimingPoint.CreateSV(next_sv.Time, intended_scroll_multiplier / bpm_multiplier))

        // now we have run out of either svs or bpms

        while bpm_i < bpm.Length do

            let next_bpm = bpm.[bpm_i]
            bpm_multiplier <- most_common_mspb / next_bpm.Data.MsPerBeat
            bpm_i <- bpm_i + 1
            result.Add(TimingPoint.CreateBPM(next_bpm.Time, next_bpm.Data.MsPerBeat, next_bpm.Data.Meter))
            result.Add(TimingPoint.CreateSV(next_bpm.Time, intended_scroll_multiplier / bpm_multiplier))

        while sv_i < sv.Length do

            let next_sv = sv.[sv_i]
            sv_i <- sv_i + 1
            intended_scroll_multiplier <- next_sv.Data
            result.Add(TimingPoint.CreateSV(next_sv.Time, intended_scroll_multiplier / bpm_multiplier))

        List.ofSeq result

    let convert (options: OsuExportOptions) (chart: Chart) (chart_meta: ChartMeta) : Beatmap =
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
                OverallDifficulty = System.Math.Round(options.OD, 1) |> max 0.0 |> min 10.0
                HPDrainRate = System.Math.Round(options.HP, 1) |> max 0.0 |> min 10.0
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

    /// Creates an .osz representation of the chart in the given folder `export_folder`
    /// If successful, the exported beatmap data + the filename of the .osz are returned
    let create_osz (options: OsuExportOptions) (chart: Chart) (chart_meta: ChartMeta) (export_folder: string) : Result<Beatmap * string, exn> =
        try
            let beatmap = OsuExport.convert options chart chart_meta

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
                | Some bg_path when File.Exists(bg_path) ->
                    use fs = File.Open(bg_path, FileMode.Open)
                    let bg_file_entry = archive.CreateEntry(beatmap.Events |> Seq.pick (function Background(bg, _, _) -> Some bg | _ -> None))
                    use bg_file_stream = bg_file_entry.Open()
                    fs.CopyTo(bg_file_stream)
                | _ -> ()

            do
                match chart_meta.Audio.Path with
                | Some audio_path when File.Exists(audio_path) ->
                    use fs =
                        File.Open(audio_path, FileMode.Open, FileAccess.ReadWrite, FileShare.ReadWrite)

                    let audio_file_entry = archive.CreateEntry(beatmap.General.AudioFilename)
                    use audio_file_stream = audio_file_entry.Open()
                    fs.CopyTo(audio_file_stream)
                | _ -> ()
            Ok (beatmap, archive_file_name)
        with err ->
            Error err