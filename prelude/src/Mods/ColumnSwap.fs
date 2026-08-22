namespace Prelude.Mods

open System
open Prelude
open Prelude.Charts

module ColumnSwap =

    let parse (s: string) : Result<int array, string> =

        if s.Length < 3 || s.Length > 10 then
            Error (sprintf "Invalid key count: %i" s.Length)
        elif not (String.forall (fun c -> c = '-' || c = '?' || Char.IsAsciiDigit c) s) then
            Error "String must in [1234567890?-]"
        else

        s |> Seq.map (function '-' -> -1 | '?' -> -2 | '0' -> 9 | c -> int (c - '1')) |> Array.ofSeq |> Ok

    let format (swap: int array) : string =
        swap
        |> Array.map (function
            | 0 -> '1'
            | 1 -> '2'
            | 2 -> '3'
            | 3 -> '4'
            | 4 -> '5'
            | 5 -> '6'
            | 6 -> '7'
            | 7 -> '8'
            | 8 -> '9'
            | 9 -> '0'
            | -2 -> '?'
            | -1 -> '-'
            | _ -> '-'
        ) |> String

    let rec private expand_chart (target_keys: int) (seed: int) (chart: ModdedChartInternal) : ModdedChartInternal =
        let random_chart = chart |> Randomise.randomise (seed) |> fst
        if random_chart.Notes.Length <> chart.Notes.Length then
            failwith "Randomised chart has different length than original chart"

        let zipped_notes = 
            (chart.Notes, random_chart.Notes)
            ||> Array.map2 (fun n1 n2 -> { Time = n1.Time; Data = Array.append n1.Data n2.Data })

        let zipped_charts = { chart with Notes = zipped_notes; Keys = chart.Keys + random_chart.Keys }

        if zipped_charts.Keys < target_keys then
            expand_chart target_keys (seed + 1) zipped_charts
        else
            zipped_charts

    let apply (swap: int array) (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        if swap.Length < 3 || swap.Length > 10 then failwithf "Invalid key count: %i" swap.Length

        let seed = 0
        let random_mark_count = swap |> Array.filter (fun x -> x = -2) |> Array.length
        let expanded_chart = if random_mark_count > 0 then expand_chart (chart.Keys + random_mark_count) seed chart else chart

        let expanded_swap, _ = 
            swap
            |> Array.mapFold (fun n x ->
                if x = -2 then
                    (chart.Keys + n, n + 1)
                else
                    (x, n)
            ) 0

        let map_row (notes: NoteType array) =
            expanded_swap
            |> Array.map (fun i -> if i >= 0 && i < notes.Length then notes.[i] else NoteType.NOTHING)

        let notes = TimeArray.map map_row expanded_chart.Notes |> TimeArray.filter (NoteRow.is_empty >> not)

        if notes.Length > 0 then
            { chart with
                Notes = notes
                Keys = swap.Length
            },
            true
        else
            chart, false

    let keys (packed_bits: int64) = int (packed_bits &&& 0x0FL)

    let pack (swap: int array) : int64 =
        if swap.Length < 3 || swap.Length > 10 then failwithf "Invalid key count: %i" swap.Length
        let mutable result = 0L
        for i = swap.Length - 1 downto 0 do
            result <- (result <<< 4) ||| int64 ((swap.[i] + 16) % 16)
        result <- (result <<< 4) ||| int64 swap.Length
        result

    let unpack (packed_bits: int64) : int array =
        let length = int (packed_bits &&& 0x0FL)
        if length < 3 || length > 10 then failwithf "Invalid key count: %i" length
        let mutable packed_bits = packed_bits
        Array.init length (fun _ ->
            packed_bits <- packed_bits >>> 4
            let value = int (packed_bits &&& 0x0FL)
            if value >= 10 then value - 16 else value
        )