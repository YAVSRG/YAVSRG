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