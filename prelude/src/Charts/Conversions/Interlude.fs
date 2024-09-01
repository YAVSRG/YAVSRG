namespace Prelude.Charts.Conversions

open System.IO
open Prelude
open Prelude.Charts.Formats.osu
open Prelude.Charts.Formats.StepMania
open Prelude.Charts

module Interlude_To_Osu =

    let private notes_to_hitobjects (notes: TimeArray<NoteRow>) keys =
        let column_to_x k =
            (float k + 0.5) * 512.0 / float keys |> int

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
                            yield 
                                HitCircle {
                                    X = column_to_x k
                                    Y = 240
                                    Time = int offset
                                    StartsNewCombo = false
                                    ColorHax = 0
                                    HitSound = HitSound.Default
                                    HitSample = HitSample.Default
                                }
                        elif nr.[k] = NoteType.HOLDHEAD then
                            yield
                                HoldNote {
                                    X = column_to_x k
                                    Y = 240
                                    Time = int offset
                                    StartsNewCombo = false
                                    ColorHax = 0
                                    HitSound = HitSound.Default
                                    EndTime = int (ln_lookahead k ss)
                                    HitSample = HitSample.Default
                                }

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
                        Some(
                            TimingPoint.Inherited { 
                                Time = int time
                                Multiplier = mult * value |> float
                                METER__UNUSED = 4
                                SampleSet = SampleSet.Soft
                                SampleIndex = 0
                                Volume = 10
                                Effects = TimingEffect.None
                            }
                        )

        let svs time1 time2 mult =
            seq {
                match corrective_sv time1 mult with
                | None -> ()
                | Some x -> yield x

                for { Time = offset; Data = value } in TimeArray.between time1 time2 sv do
                    yield 
                        TimingPoint.Inherited {
                            Time = int offset
                            Multiplier = mult * value |> float
                            METER__UNUSED = 4
                            SampleSet = SampleSet.Soft
                            SampleIndex = 0
                            Volume = 10
                            Effects = TimingEffect.None
                        }
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
                            yield 
                                TimingPoint.Uninherited {
                                    Time = int offset
                                    MsPerBeat = mspb |> float
                                    Meter = int meter
                                    SampleSet = SampleSet.Soft
                                    SampleIndex = 0
                                    Volume = 10
                                    Effects = TimingEffect.None
                                }
                            yield! svs offset offset2 (most_common_mspb / mspb)
                            bs <- List.tail bs
                        | {
                              Time = offset
                              Data = {
                                         Meter = meter
                                         MsPerBeat = mspb
                                     }
                          } :: [] ->
                            yield 
                                TimingPoint.Uninherited {
                                    Time = int offset
                                    MsPerBeat = mspb |> float
                                    Meter = int meter
                                    SampleSet = SampleSet.Soft
                                    SampleIndex = 0
                                    Volume = 10
                                    Effects = TimingEffect.None
                                }
                            yield! svs offset Time.infinity (most_common_mspb / mspb)
                            bs <- List.tail bs
                        | [] -> failwith "impossible by loop condition"
            }

        tps |> List.ofSeq

    let convert (chart: Chart) : Beatmap =
        let general : General =
            {
                AudioFilename =
                    match chart.Header.AudioFile with
                    | Relative s -> s
                    | Absolute s -> Path.GetFileName s
                    | Asset _
                    | Missing -> "audio.mp3"
                AudioLeadIn = 0
                PreviewTime = int chart.Header.PreviewTime
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
                Title = chart.Header.Title
                TitleUnicode = Option.defaultValue chart.Header.Title chart.Header.TitleNative
                Artist = chart.Header.Artist
                ArtistUnicode = Option.defaultValue chart.Header.Artist chart.Header.ArtistNative
                Creator = chart.Header.Creator
                Version = chart.Header.DiffName
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
                        (match chart.Header.BackgroundFile with
                         | Relative s -> s
                         | Absolute s -> Path.GetFileName s
                         | Asset _
                         | Missing -> "bg.png"),
                        0, 0
                    )
                ]
            Objects = notes_to_hitobjects chart.Notes chart.Keys
            Timing = convert_timing_points chart.BPM chart.SV (Chart.find_most_common_bpm chart)
        }

module Interlude_To_StepMania =

    let convert (chart: Chart) : StepManiaData = failwith "nyi"
