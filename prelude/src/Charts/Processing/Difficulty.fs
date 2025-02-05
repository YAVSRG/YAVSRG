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

(*
    Difficulty calculation
    To be one day scrapped as it gets overshadowed by a better system for picking what to play
    Is just a port of the original C# version I wrote when I was 18
*)

type DifficultyRating =
    {
        Physical: float
        PhysicalData: float array
        PhysicalComposite: float array2d
    }

module DifficultyRating =

    let jack_curve delta =
        let width_scale = 0.02
        let height_scale = 26.3
        let curve_exp = 1.0
        Math.Min(height_scale / Math.Pow(width_scale * float delta, curve_exp), 20.0)

    let stream_curve delta =
        let width_scale = 0.02
        let height_scale = 13.7
        let curve_exp = 1.0
        let cutoff = 10.0

        Math.Max(
            (height_scale / Math.Pow(width_scale * float delta, curve_exp)
             - 0.1 * height_scale / Math.Pow(width_scale * float delta, curve_exp * cutoff)),
            0.0
        )

    let jack_compensation (jack_delta: GameplayTime) (stream_delta: GameplayTime) =
        Math.Min(Math.Pow(Math.Max(Math.Log(float (jack_delta / stream_delta), 2.0), 0.0), 2.0), 1.0)

    let private root_mean_power values power =
        match values with
        | x :: [] -> x
        | [] -> 0.0
        | xs ->
            let (count, sumpow) =
                List.fold (fun (count, a) b -> (count + 1.0, a + Math.Pow(b, power))) (0.0, 0.0) xs

            Math.Pow(sumpow / count, 1.0 / power)

    let stamina_func value input (delta: GameplayTime) =
        let stamina_base_func ratio = 1.0 + 0.105 * ratio
        let stamina_decay_func delta = Math.Exp(-0.00044 * delta)
        let v = Math.Max(value * stamina_decay_func (float delta), 0.01)
        v * stamina_base_func (input / v)

    let private overall_difficulty arr =
        Math.Pow(Array.fold (fun v x -> v * Math.Exp(0.01 * Math.Max(0.0, Math.Log(x / v)))) 0.01 arr, 0.6)
        * 2.5

    let private OHTNERF = 3.0
    let private SCALING_VALUE = 0.55

    let private calculate_uncached (rate: Rate) (notes: TimeArray<NoteRow>) : DifficultyRating =

        let keys = notes.[0].Data.Length

        let hand_split = Layout.keys_on_left_hand keys

        let fingers = Array.zeroCreate<Time> keys

        let physical_composite = Array2D.zeroCreate notes.Length keys

        let update_note_difficulty (column: int, index: int, time: Time) =

            let jack_delta, jack_v =
                if fingers.[column] > 0.0f<ms> then
                    let delta = (time - fingers.[column]) / rate
                    delta, Math.Pow(jack_curve delta, OHTNERF)
                else
                    10000.0f<ms / rate>, 0.0

            let mutable trill = 0.0

            let hand_lo, hand_hi =
                if column < hand_split then 0, hand_split - 1 else hand_split, keys - 1

            for k = hand_lo to hand_hi do
                if k <> column && fingers.[k] > 0.0f<ms> then
                    let trill_delta = (time - fingers.[k]) / rate
                    trill <- trill + Math.Pow((stream_curve trill_delta) * (jack_compensation jack_delta trill_delta), OHTNERF)

            physical_composite.[index, column] <- Math.Pow(trill + jack_v, 1.0 / OHTNERF)

        let snap_difficulty (strain: float array) mask =
            let mutable vals = []

            for k in Bitmask.toSeq mask do
                vals <- strain.[k] :: vals

            root_mean_power vals 1.0

        let physical_data = Array.zeroCreate notes.Length
        let current_strain = Array.zeroCreate<float> keys
        let mutable i = 0

        for { Time = offset; Data = nr } in notes do
            let left_hand_hits =
                seq {
                    for k = 0 to hand_split - 1 do
                        if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                            yield k
                }
                |> Bitmask.ofSeq

            let right_hand_hits =
                seq {
                    for k = hand_split to keys - 1 do
                        if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                            yield k
                }
                |> Bitmask.ofSeq

            let hand (hand_hits: Bitmask) =
                for k in Bitmask.toSeq hand_hits do
                    update_note_difficulty (k, i, offset)

                    current_strain.[k] <-
                        stamina_func
                            (current_strain.[k])
                            (physical_composite.[i, k] * SCALING_VALUE)
                            ((offset - fingers.[k]) / rate)

                for k in Bitmask.toSeq hand_hits do
                    fingers.[k] <- offset

            hand left_hand_hits
            hand right_hand_hits

            physical_data.[i] <- snap_difficulty current_strain (left_hand_hits ||| right_hand_hits)

            i <- i + 1

        let physical = overall_difficulty physical_data

        {
            Physical = if Double.IsFinite physical then physical else 0.0
            PhysicalData = physical_data
            PhysicalComposite = physical_composite
        }

    let calculate = calculate_uncached |> cached

    let physical_color v =
        try
            let a = Math.Min(1.0, v * 0.1)
            let b = Math.Min(1.0, Math.Max(1.0, v * 0.1) - 1.0)
            Color.FromArgb(255.0 * a |> int, 255.0 * (1.0 - a) |> int, 255.0 * b |> int)
        with _ ->
            Color.Red