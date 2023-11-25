namespace Prelude.Charts.Conversions

open System.IO
open Prelude
open Prelude.Charts.Formats.``osu!``
open Prelude.Charts.Formats.StepMania
open Prelude.Charts

module Interlude_To_Osu =

    let private notes_to_hitobjects (notes: TimeArray<NoteRow>) keys =
        let column_to_x k =
            (float k + 0.5) * 512.0 / float keys |> round

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
                            yield HitCircle((column_to_x k, 240.0), offset, enum 0, (enum 0, enum 0, 0, 0, ""))
                        elif nr.[k] = NoteType.HOLDHEAD then
                            yield
                                HoldNote(
                                    (column_to_x k, 240.0),
                                    offset,
                                    ln_lookahead k ss,
                                    enum 0,
                                    (enum 0, enum 0, 0, 0, "")
                                )

                    yield! convert ss
                | [] -> ()
            }

        convert (notes |> Array.toList) |> List.ofSeq

    let private convert_timing_points (bpm: TimeArray<BPM>) (sv: TimeArray<float32>) (most_common_mspb: float32<ms/beat>) =

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
                        Some(TimingPoint.SV(offset, mult * value, (SampleSet.Soft, 0, 10), enum 0))

        let svs time1 time2 mult =
            seq {
                match corrective_sv time1 mult with
                | None -> ()
                | Some x -> yield x

                for { Time = offset; Data = value } in TimeArray.between time1 time2 sv do
                    yield TimingPoint.SV(offset, mult * value, (SampleSet.Soft, 0, 10), enum 0)
            }

        let tps =
            seq {
                let mutable bs = bpm |> List.ofArray

                if List.isEmpty bs then ()
                else

                    yield! svs (-Time.infinity) (List.head bs).Time 1.0f

                    while not (List.isEmpty bs) do
                        match bs with
                        | {
                              Time = offset
                              Data = {
                                         Meter = meter
                                         MsPerBeat = mspb
                                     }
                          } :: { Time = offset2 } :: rs ->
                            yield TimingPoint.BPM(offset, mspb, meter, (SampleSet.Soft, 0, 10), enum 0)
                            yield! svs offset offset2 (most_common_mspb / mspb)
                            bs <- List.tail bs
                        | {
                              Time = offset
                              Data = {
                                         Meter = meter
                                         MsPerBeat = beatLength
                                     }
                          } :: [] ->
                            yield TimingPoint.BPM(offset, beatLength, meter, (SampleSet.Soft, 0, 10), enum 0)
                            yield! svs offset Time.infinity (most_common_mspb / beatLength)
                            bs <- List.tail bs
                        | [] -> failwith "impossible by loop condition"
            }

        tps |> List.ofSeq

    let convert (chart: Chart) : Beatmap =
        let general =
            { General.Default with
                AudioFilename =
                    match chart.Header.AudioFile with
                    | Relative s -> s
                    | Absolute s -> Path.GetFileName s
                    | Asset _
                    | Missing -> "audio.mp3"
                PreviewTime = chart.Header.PreviewTime
                SampleSet = SampleSet.Soft
                Mode = GameMode.Mania
            }

        let editor = Editor.Default

        let meta =
            { Metadata.Default with
                Title = chart.Header.Title
                TitleUnicode = Option.defaultValue chart.Header.Title chart.Header.TitleNative
                Artist = chart.Header.Artist
                ArtistUnicode = Option.defaultValue chart.Header.Artist chart.Header.ArtistNative
                Creator = chart.Header.Creator
                Version = chart.Header.DiffName
            }

        let diff =
            { Difficulty.Default with
                CircleSize = float chart.Keys
                OverallDifficulty = 8.0
                HPDrainRate = 8.0
            }

        {
            General = general
            Editor = editor
            Metadata = meta
            Difficulty = diff
            Events =
                [
                    Background(
                        (match chart.Header.BackgroundFile with
                         | Relative s -> s
                         | Absolute s -> Path.GetFileName s
                         | Asset _
                         | Missing -> "bg.png"),
                        (0.0, 0.0)
                    )
                ]
            Objects = notes_to_hitobjects chart.Notes chart.Keys
            Timing = convert_timing_points chart.BPM chart.SV (Chart.find_most_common_bpm chart)
        }

module Interlude_To_StepMania =

    let convert (chart: Chart) : StepManiaData = failwith "nyi"