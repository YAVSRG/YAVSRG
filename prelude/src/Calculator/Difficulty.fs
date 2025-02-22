namespace Prelude.Calculator

open System
open Prelude
open Prelude.Charts

module Layout =

    let keys_on_left_hand (keymode: int) =
        match keymode with
        | 3 -> 2
        | 4 -> 2
        | 5 -> 3
        | 6 -> 3
        | 7 -> 4
        | 8 -> 4
        | 9 -> 5
        | 10 -> 5
        | _ -> failwithf "Invalid keymode %i" keymode

[<Struct>]
type NoteDifficulty =
    {
        mutable J: float32
        mutable SL: float32
        mutable SR: float32
        mutable T: float32
    }

type Difficulty =
    {
        NoteDifficulty: NoteDifficulty array array
        Strain: (float32 array * float32) array
        Overall: float
    }

module Difficulty =

    /// Cutoff of `JACK_CURVE_CUTOFF` prevents stacked minijacks from slingshotting the perceived BPM
    /// Especially since you can typically hit such minijacks with good accuracy by playing them as a 230 jack no matter the BPM
    let JACK_CURVE_CUTOFF = 230.0f
    /// Converts ms difference between notes (in the same column) into its equivalent BPM
    /// (on 1/4 snap, hence 1 minute * 1/4 is numerator)
    let ms_to_jack_bpm (delta: GameplayTime) =
        15000.0f<ms / rate> / delta
        |> min JACK_CURVE_CUTOFF

    /// These variables adjust what BPM the stream curve cuts off at
    /// Best to put this whole expression into https://www.desmos.com/calculator + variables as sliders if you want to understand it better
    let STREAM_CURVE_CUTOFF = 10.0f
    let STREAM_CURVE_CUTOFF_2 = 10.0f
    /// Converts ms difference between notes (in the same column) into its equivalent BPM (on 1/4 snap)
    ///
    /// Once the ms gets low enough, the curve starts going back down towards 0
    /// since adjacent notes that would make a "500bpm stream" can be hit as grace notes and don't add difficulty
    /// The real threshold is lower than 500 though, and controlled by the cutoff variables
    let ms_to_stream_bpm (delta: GameplayTime) =
        300.0f / (0.02f<rate / ms> * delta) -
        300.0f / MathF.Pow(0.02f<rate / ms> * delta, STREAM_CURVE_CUTOFF) / STREAM_CURVE_CUTOFF_2
        |> max 0.0f

    /// Consider note A in column 1, note B also in column 1, 100ms earlier and note C in column 2, also 100ms earlier
    /// Note A should not get any "stream" value because you will be using your wrist to tap the key again, right after note B
    ///
    /// This function uses the ratio between the jack and stream spacing to determine a multiplier between 0.0 and 1.0
    /// Example: for [12][1] as described above, 0.0 is returned to fully cancel out the stream value
    /// Example: for [1][2][1] evenly spaced trill (note C is 50ms earlier instead), 1.0 is returned to do no cancelling at all
    /// Best to put this whole expression into https://www.desmos.com/calculator if you want to understand it better
    let jack_compensation (jack_delta: GameplayTime) (stream_delta: GameplayTime) =
        let ratio = jack_delta / stream_delta
        MathF.Log(ratio, 2.0f)
        |> max 0.0f
        |> sqrt
        |> min 1.0f

    /// Walks forward through each row of notes in a chart:
    /// - Sets JF on each note ("jack-forward") to the BPM between it and the previous note in its column
    /// - Sets SLF on each note ("stream-left-forward") to the BPM betwen it and the previous note in the column to the left, if this column exists and is on the same hand
    /// - Sets SRF on each note ("stream-right-forward") to the BPM betwen it and the previous note in the column to the right, if this column exists and is on the same hand
    /// Don't worry about what it does for keymodes above 4K for SLF & SLR, I'm redesigning it
    let private notes_difficulty_pass (rate: Rate, notes: TimeArray<NoteRow>) (data: NoteDifficulty array array) =
        let keys = notes.[0].Data.Length
        let hand_split = Layout.keys_on_left_hand keys

        let last_note_in_column = Array.create<Time> keys ((TimeArray.first notes).Value.Time - 1000000.0f<ms>)

        let note_difficulty (i: int, k: int, time: Time) =
            let jack_delta =
                let delta = (time - last_note_in_column.[k]) / rate
                data.[i].[k].J <- ms_to_jack_bpm delta
                delta

            let hand_lo, hand_hi =
                if k < hand_split then
                    0, hand_split - 1
                else
                    hand_split, keys - 1

            let mutable sl = 0.0f
            let mutable sr = 0.0f
            for hand_k = hand_lo to hand_hi do
                if hand_k <> k then
                    let trill_delta = (time - last_note_in_column.[hand_k]) / rate
                    let trill_v = ms_to_stream_bpm trill_delta * jack_compensation jack_delta trill_delta
                    if hand_k < k then
                        sl <- sl + trill_v
                    else
                        sr <- sr + trill_v

            data.[i].[k].SL <- sl
            data.[i].[k].SR <- sr

        for i = 0 to notes.Length - 1 do
            let { Time = time; Data = nr } = notes.[i]

            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    note_difficulty (i, k, time)

            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    last_note_in_column.[k] <- time

    let stamina_func (value: float32) (input: float32) (delta: GameplayTime) =
        let stamina_base_func ratio = 1.0f + 0.105f * ratio
        let stamina_decay_func (delta: GameplayTime) = MathF.Exp(-0.00044f</ms*rate> * delta)
        let v = Math.Max(value * stamina_decay_func delta, 0.01f)
        v * stamina_base_func (input / v)

    let private OHTNERF = 3.0f
    let STREAM_CURVE_HEIGHT_SCALE = 0.5209125f

    let private finger_strain_pass (note_difficulty: NoteDifficulty array array) (rate: Rate, notes: TimeArray<NoteRow>) =
        let keys = notes.[0].Data.Length

        let strain = Array.zeroCreate<float32> keys
        let last_note_in_column = Array.zeroCreate<Time> keys

        seq {
            for i = 0 to notes.Length - 1 do
                let { Time = offset; Data = nr } = notes.[i]

                let mutable sum = 0.0f
                let mutable n = 0.0f
                for k = 0 to keys - 1 do
                    if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then

                        note_difficulty.[i].[k].T <-
                            MathF.Pow(
                                MathF.Pow(STREAM_CURVE_HEIGHT_SCALE * note_difficulty.[i].[k].SL, OHTNERF) +
                                MathF.Pow(STREAM_CURVE_HEIGHT_SCALE * note_difficulty.[i].[k].SR, OHTNERF) +
                                MathF.Pow(note_difficulty.[i].[k].J, OHTNERF),
                                1.0f / OHTNERF
                            )

                        strain.[k] <-
                            stamina_func
                                strain.[k]
                                note_difficulty.[i].[k].T
                                ((offset - last_note_in_column.[k]) / rate)
                        last_note_in_column.[k] <- offset

                        sum <- sum + strain.[k]
                        n <- n + 1.0f

                yield Array.copy strain, if n = 0.0f then 0.0f else sum / n
        }
        |> Array.ofSeq

    let CURVE_POWER = 0.6f
    let CURVE_SCALE = 0.4056f

    let private overall_difficulty_pass (finger_strain_data: (float32 array * float32) seq) =
        let mutable v = 0.01f
        for _, x in finger_strain_data do
            v <- v * MathF.Exp(0.01f * Math.Max(0.0f, MathF.Log(x / v)))

        MathF.Pow(v, CURVE_POWER) * CURVE_SCALE

    let private overall_difficulty_pass_v2 (finger_strain_data: (float32 array * float32) array) =

        let length = float32 finger_strain_data.Length
        let weight_func = fun (i: int) -> (float32 i / length) ** 2.0f

        let mutable weight = 0.0f
        let mutable total = 0.0f

        for i, value in finger_strain_data |> Seq.map snd |> Seq.filter (fun x -> x > 0.0f) |> Seq.sort |> Seq.indexed do
            let w = weight_func i
            weight <- weight + w
            total <- total + value * w
        MathF.Pow(total / weight, CURVE_POWER) * CURVE_SCALE

    let private calculate_uncached (rate: Rate, notes: TimeArray<NoteRow>) : Difficulty =
        let keys = notes.[0].Data.Length
        let note_data = Array.init notes.Length (fun _ -> Array.zeroCreate keys)
        notes_difficulty_pass (rate, notes) note_data
        let physical_data = finger_strain_pass note_data (rate, notes)
        let physical = overall_difficulty_pass physical_data

        {
            NoteDifficulty = note_data
            Strain = physical_data
            Overall = if Single.IsFinite physical then float physical else 0.0
        }

    let calculate = calculate_uncached |> cached

    let color v =
        try
            let a = Math.Min(1.0, v * 0.1)
            let b = Math.Min(1.0, Math.Max(1.0, v * 0.1) - 1.0)
            Color.FromArgb(255.0 * a |> int, 255.0 * (1.0 - a) |> int, 255.0 * b |> int)
        with _ ->
            Color.Red