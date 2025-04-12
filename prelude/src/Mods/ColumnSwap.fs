namespace Prelude.Mods

open System
open Prelude
open Prelude.Charts

module ColumnSwap =

    let parse (s: string) : Result<int array, string> =

        if s.Length < 3 || s.Length > 10 then
            Error (sprintf "Invalid key count: %i" s.Length)
        elif not (String.forall (fun c -> c = '-' || Char.IsAsciiDigit c) s) then
            Error "String must be all digits or '-'"
        else

        s |> Seq.map (function '-' -> -1 | c -> int (c - '0')) |> Array.ofSeq |> Ok

    let apply (swap: int array) (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        if swap.Length < 3 || swap.Length > 10 then failwithf "Invalid key count: %i" swap.Length

        let map_row (notes: NoteType array) =
            swap |> Array.map (fun i -> if i >= 0 && i < notes.Length then notes.[i] else NoteType.NOTHING)

        let notes = TimeArray.map map_row chart.Notes |> TimeArray.filter (NoteRow.is_empty >> not)

        if notes.Length > 0 then
            { chart with
                Notes = notes
                Keys = swap.Length
            },
            true
        else
            chart, false

    let pack (swap: int array) : int64 =
        if swap.Length < 3 || swap.Length > 10 then failwithf "Invalid key count: %i" swap.Length
        let mutable result = 0L
        for i = swap.Length - 1 downto 0 do
            result <- (result <<< 4) ||| (if swap.[i] < 0 then 0x0FL else int64 swap.[i])
        result <- (result <<< 4) ||| int64 swap.Length
        result

    let unpack (packed_bits: int64) : int array =
        let length = int (packed_bits &&& 0x0FL)
        if length < 3 || length > 10 then failwithf "Invalid key count: %i" length
        let mutable packed_bits = packed_bits
        Array.init length (fun _ ->
            packed_bits <- packed_bits >>> 4
            let i = int (packed_bits &&& 0x0FL)
            if i = 0x0F then -1 else i
        )