namespace Prelude.Charts.Processing.Difficulty

open System
open Prelude
open Prelude.Charts

(*
    Representation of hand/keyboard layouts.
    Used in the difficulty calculation to model how some things strain your hand(s) differently depending on which fingers are used
*)

type Layout =
    | Spread = 0
    | OneHand = 1
    | LeftOne = 2
    | RightOne = 3
    | LeftTwo = 4
    | RightTwo = 5
    | BMSLeft = 6
    | BMSRight = 7

module Layout =

    type Hand = int list
    type LayoutInfo = Hand list

    let get_finger_position k h =
        let rec f k h i =
            match h with
            | n :: fs -> if n = k then i else f k fs (i + 1)
            | [] -> -1

        f k h 0

    let get_hand_bit_mask (h: Hand) = h |> Seq.ofList |> Bitmask.ofSeq

    let info (l, keycount) : LayoutInfo option =
        match (l, keycount) with
        | (Layout.OneHand, 3) -> Some [ [ 0; 1; 2 ] ]
        | (Layout.LeftOne, 3) -> Some [ [ 0; 1 ]; [ 2 ] ]
        | (Layout.RightOne, 3) -> Some [ [ 0 ]; [ 1; 2 ] ]

        | (Layout.OneHand, 4) -> Some [ [ 0; 1; 2; 3 ] ]
        | (Layout.Spread, 4) -> Some [ [ 0; 1 ]; [ 2; 3 ] ]
        | (Layout.LeftOne, 4) -> Some [ [ 0; 1; 2 ]; [ 3 ] ]
        | (Layout.RightOne, 4) -> Some [ [ 0 ]; [ 1; 2; 3 ] ]

        | (Layout.OneHand, 5) -> Some [ [ 0; 1; 2; 3; 4 ] ]
        | (Layout.LeftOne, 5) -> Some [ [ 0; 1; 2 ]; [ 3; 4 ] ]
        | (Layout.RightOne, 5) -> Some [ [ 0; 1 ]; [ 2; 3; 4 ] ]
        | (Layout.LeftTwo, 5) -> Some [ [ 0; 1; 2; 3 ]; [ 4 ] ]
        | (Layout.RightTwo, 5) -> Some [ [ 0 ]; [ 1; 2; 3; 4 ] ]

        | (Layout.Spread, 6) -> Some [ [ 0; 1; 2 ]; [ 3; 4; 5 ] ]
        | (Layout.LeftOne, 6) -> Some [ [ 0; 1; 2; 3 ]; [ 4; 5 ] ]
        | (Layout.RightOne, 6) -> Some [ [ 0; 1 ]; [ 2; 3; 4; 5 ] ]
        | (Layout.LeftTwo, 6) -> Some [ [ 0; 1; 2; 3; 4 ]; [ 5 ] ]
        | (Layout.RightTwo, 6) -> Some [ [ 0 ]; [ 1; 2; 3; 4; 5 ] ]

        | (Layout.LeftOne, 7) -> Some [ [ 0; 1; 2; 3 ]; [ 4; 5; 6 ] ]
        | (Layout.RightOne, 7) -> Some [ [ 0; 1; 2 ]; [ 3; 4; 5; 6 ] ]
        | (Layout.LeftTwo, 7) -> Some [ [ 0; 1; 2; 3; 4 ]; [ 5; 6 ] ]
        | (Layout.RightTwo, 7) -> Some [ [ 0; 1 ]; [ 2; 3; 4; 5; 6 ] ]
        | (Layout.BMSLeft, 7) -> Some [ [ 0; 1; 3; 2 ]; [ 4; 5; 6 ] ]
        | (Layout.BMSRight, 7) -> Some [ [ 0; 1; 2 ]; [ 4; 3; 5; 6 ] ]

        | (Layout.Spread, 8) -> Some [ [ 0; 1; 2; 3 ]; [ 4; 5; 6; 7 ] ]
        | (Layout.LeftOne, 8) -> Some [ [ 0; 1; 2 ]; [ 3; 4; 5; 6; 7 ] ]
        | (Layout.RightOne, 8) -> Some [ [ 0; 1; 2; 3; 4 ]; [ 5; 6; 7 ] ]

        | (Layout.LeftOne, 9) -> Some [ [ 0; 1; 2; 3; 4 ]; [ 5; 6; 7; 8 ] ]
        | (Layout.RightOne, 9) -> Some [ [ 0; 1; 2; 3 ]; [ 4; 5; 6; 7; 8 ] ]

        | (Layout.Spread, 10) -> Some [ [ 0; 1; 2; 3; 4 ]; [ 5; 6; 7; 8; 9 ] ]

        | _ -> None

    let name (l, keycount) =
        match (l, keycount) with
        | (Layout.OneHand, 3) -> "One-Handed"
        | (Layout.LeftOne, 3) -> "2k+1"
        | (Layout.RightOne, 3) -> "1k+2"

        | (Layout.OneHand, 4) -> "One-Handed"
        | (Layout.Spread, 4) -> "Spread"
        | (Layout.LeftOne, 4) -> "3k+1"
        | (Layout.RightOne, 4) -> "1k+3"

        | (Layout.OneHand, 5) -> "One-Handed"
        | (Layout.LeftOne, 5) -> "3k+2"
        | (Layout.RightOne, 5) -> "2k+3"
        | (Layout.LeftTwo, 5) -> "4k+1"
        | (Layout.RightTwo, 5) -> "1k+4"

        | (Layout.Spread, 6) -> "Spread"
        | (Layout.LeftOne, 6) -> "4k+2"
        | (Layout.RightOne, 6) -> "2k+4"
        | (Layout.LeftTwo, 6) -> "5k+1"
        | (Layout.RightTwo, 6) -> "1k+5"

        | (Layout.LeftOne, 7) -> "Left Thumb"
        | (Layout.RightOne, 7) -> "Right Thumb"
        | (Layout.LeftTwo, 7) -> "5k+2"
        | (Layout.RightTwo, 7) -> "2k+5"
        | (Layout.BMSLeft, 7) -> "IIDX Left Thumb"
        | (Layout.BMSRight, 7) -> "IIDX Right Thumb"

        | (Layout.Spread, 8) -> "Spread"
        | (Layout.LeftOne, 8) -> "5k+3"
        | (Layout.RightOne, 8) -> "3k+5"

        | (Layout.LeftOne, 9) -> "Left Thumb"
        | (Layout.RightOne, 9) -> "Right Thumb"

        | (Layout.Spread, 10) -> "Spread"

        | _ -> "Unknown Layout"

    let list (k: int) =
        [
            (Layout.Spread, k)
            (Layout.OneHand, k)
            (Layout.LeftOne, k)
            (Layout.RightOne, k)
            (Layout.LeftTwo, k)
            (Layout.RightTwo, k)
            (Layout.BMSLeft, k)
            (Layout.BMSRight, k)
        ]
        |> List.filter (info >> Option.isSome)
        |> List.map (fun (a, b) -> a)

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

    let private jack_curve delta =
        let width_scale = 0.02
        let height_scale = 26.3
        let curve_exp = 1.0
        Math.Min(height_scale / Math.Pow(width_scale * float delta, curve_exp), 20.0)

    let private stream_curve delta =
        let width_scale = 0.02
        let height_scale = 13.7
        let curve_exp = 1.0
        let cutoff = 10.0

        Math.Max(
            (height_scale / Math.Pow(width_scale * float delta, curve_exp)
             - 0.1 * height_scale / Math.Pow(width_scale * float delta, curve_exp * cutoff)),
            0.0
        )

    let private jack_compensation (jack_delta: GameplayTime) (stream_delta: GameplayTime) =
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

        let layout =
            Layout.list keys
            |> List.head
            |> fun l -> Layout.info (l, keys) |> fun x -> x.Value

        let fingers = Array.zeroCreate<Time> keys

        let physical_composite = Array2D.zeroCreate notes.Length keys

        let update_note_difficulty (column: int, index: int, time: Time, other_columns: Bitmask) =
            let s = other_columns |> Bitmask.unset_key column

            let jack_delta, jack_v =
                if fingers.[column] > 0.0f<ms> then
                    let delta = (time - fingers.[column]) / rate
                    delta, Math.Pow(jack_curve delta, OHTNERF)
                else
                    10000.0f<ms / rate>, 0.0

            let mutable trill = 0.0

            for k in Bitmask.toSeq s do
                if fingers.[k] > 0.0f<ms> then
                    let trill_delta = (time - fingers.[k]) / rate
                    trill <- trill + Math.Pow((stream_curve trill_delta) * (jack_compensation jack_delta trill_delta), OHTNERF)

            physical_composite.[index, column] <- Math.Pow(trill + jack_v, 1.0 / OHTNERF)

        let snap_difficulty (strain: float array) mask =
            let mutable vals = []

            for k in Bitmask.toSeq mask do
                vals <- strain.[k] :: vals

            root_mean_power vals 1.0

        let physical_data = Array.zeroCreate notes.Length
        let last_hand_use = Array.zeroCreate (List.length layout)
        let current_strain = Array.zeroCreate<float> keys
        let mutable i = 0

        for { Time = offset; Data = nr } in notes do
            let hits =
                seq {
                    for k = 0 to keys - 1 do
                        if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                            yield k
                }
                |> Bitmask.ofSeq

            if hits > 0us then
                List.fold
                    (fun h hand ->
                        let hand_hits = hits &&& Layout.get_hand_bit_mask hand

                        for k in Bitmask.toSeq hand_hits do
                            update_note_difficulty (k, i, offset, Layout.get_hand_bit_mask hand)

                            current_strain.[k] <-
                                stamina_func
                                    (current_strain.[k])
                                    (physical_composite.[i, k] * SCALING_VALUE)
                                    ((offset - fingers.[k]) / rate)

                        for k in Bitmask.toSeq hand_hits do
                            fingers.[k] <- offset

                        last_hand_use.[h] <- offset
                        h + 1
                    )
                    0
                    layout
                |> ignore

                physical_data.[i] <- snap_difficulty current_strain hits

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