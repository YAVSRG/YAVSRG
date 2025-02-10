namespace Prelude.Charts.Processing.Difficulty

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

type DifficultyRating =
    {
        NoteDifficulty: NoteDifficulty array array
        Strain: (float32 array * float32) array
        Overall: float
    }

module DifficultyRating =

    let jack_curve (delta: GameplayTime) =
        let width_scale = 0.02f</ms*rate>
        let height_scale = 26.3f
        Math.Min(height_scale / (width_scale * delta), 20.0f)

    let stream_curve (delta: GameplayTime) =
        let width_scale = 0.02f</ms*rate>
        let height_scale = 13.7f
        let cutoff = 10.0f

        Math.Max(
            (height_scale / (width_scale * delta)
             - 0.1f * height_scale / MathF.Pow(width_scale * delta, cutoff)),
            0.0f
        )

    let jack_compensation (jack_delta: GameplayTime) (stream_delta: GameplayTime) =
        Math.Min(MathF.Pow(Math.Max(MathF.Log((jack_delta / stream_delta), 2.0f), 0.0f), 2.0f), 1.0f)

    let private OHTNERF = 3.0f

    // todo: break down into smaller info: left, right, jack, LN
    // todo: backwards pass as well
    let private notes_difficulty_pass (rate: Rate, notes: TimeArray<NoteRow>) : NoteDifficulty array array =
        let keys = notes.[0].Data.Length
        let hand_split = Layout.keys_on_left_hand keys

        let last_note_in_column = Array.zeroCreate<Time> keys

        let output = Array.init notes.Length (fun _ -> Array.zeroCreate keys)

        let note_difficulty (i: int, k: int, time: Time) =
            let jack_delta =
                if last_note_in_column.[k] > 0.0f<ms> then
                    let delta = (time - last_note_in_column.[k]) / rate
                    output.[i].[k].J <- MathF.Pow(jack_curve delta, OHTNERF)
                    delta
                else
                    10000.0f<ms / rate>

            let hand_lo, hand_hi =
                if k < hand_split then
                    0, hand_split - 1
                else
                    hand_split, keys - 1

            for hand_k = hand_lo to hand_hi do
                if hand_k <> k && last_note_in_column.[hand_k] > 0.0f<ms> then
                    let trill_delta = (time - last_note_in_column.[hand_k]) / rate
                    let trill_v = MathF.Pow((stream_curve trill_delta) * (jack_compensation jack_delta trill_delta), OHTNERF)
                    if hand_k < k then
                        output.[i].[k].SL <- output.[i].[k].SL + trill_v
                    else
                        output.[i].[k].SR <- output.[i].[k].SR + trill_v

            output.[i].[k].T <- MathF.Pow(output.[i].[k].SL + output.[i].[k].SR + output.[i].[k].J, 1.0f / OHTNERF)

        for i = 0 to notes.Length - 1 do
            let { Time = time; Data = nr } = notes.[i]

            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    note_difficulty (i, k, time)

            for k = 0 to keys - 1 do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    last_note_in_column.[k] <- time

        output

    let stamina_func (value: float32) (input: float32) (delta: GameplayTime) =
        let stamina_base_func ratio = 1.0f + 0.105f * ratio
        let stamina_decay_func (delta: GameplayTime) = MathF.Exp(-0.00044f</ms*rate> * delta)
        let v = Math.Max(value * stamina_decay_func delta, 0.01f)
        v * stamina_base_func (input / v)

    let private SCALING_VALUE = 0.55f

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
                        strain.[k] <-
                            stamina_func
                                (strain.[k])
                                (note_difficulty.[i].[k].T * SCALING_VALUE)
                                ((offset - last_note_in_column.[k]) / rate)

                        last_note_in_column.[k] <- offset

                        sum <- sum + strain.[k]
                        n <- n + 1.0f

                yield Array.copy strain, if n = 0.0f then 0.0f else sum / n
        }
        |> Array.ofSeq

    let private overall_difficulty_pass (finger_strain_data: (float32 array * float32) seq) =
        let mutable v = 0.01f
        for _, x in finger_strain_data do
            v <- v * MathF.Exp(0.01f * Math.Max(0.0f, MathF.Log(x / v)))

        MathF.Pow(v, 0.6f) * 2.5f

    let private overall_difficulty_pass_v2 (finger_strain_data: (float32 array * float32) array) =

        let length = float32 finger_strain_data.Length
        let weight_func = fun (i: int) -> (float32 i / length) ** 2.0f

        let mutable weight = 0.0f
        let mutable total = 0.0f

        for i, value in finger_strain_data |> Seq.map snd |> Seq.filter (fun x -> x > 0.0f) |> Seq.sort |> Seq.indexed do
            let w = weight_func i
            weight <- weight + w
            total <- total + value * w
        MathF.Pow(total / weight, 0.6f) * 2.5f

    let private difficulty_distribution (finger_strain_data: (float32 array * float32) array) =
        finger_strain_data
        |> Seq.map snd
        |> Seq.filter (fun x -> x > 0.0f)
        |> Seq.countBy (fun x -> floor(x * 10.0f) / 10.0f)
        |> Seq.sortBy fst

    let private calculate_uncached (rate: Rate, notes: TimeArray<NoteRow>) : DifficultyRating =

        let physical_composite = notes_difficulty_pass(rate, notes)
        let physical_data = finger_strain_pass physical_composite (rate, notes)
        let physical = overall_difficulty_pass physical_data

        {
            NoteDifficulty = physical_composite
            Strain = physical_data
            Overall = if Single.IsFinite physical then float physical else 0.0
        }

    let calculate = calculate_uncached |> cached

    let physical_color v =
        try
            let a = Math.Min(1.0, v * 0.1)
            let b = Math.Min(1.0, Math.Max(1.0, v * 0.1) - 1.0)
            Color.FromArgb(255.0 * a |> int, 255.0 * (1.0 - a) |> int, 255.0 * b |> int)
        with _ ->
            Color.Red