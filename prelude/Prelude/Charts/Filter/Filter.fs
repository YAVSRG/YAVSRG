module Prelude.Charts.Filter

open System
open Prelude.Charts.Interlude

type Bitmapping =
    | Column of int
    | Not of Bitmapping
    | True
    member this.Evaluate(bitmap : Bitmap) =
        match this with
        | Column i -> hasBit i bitmap
        | Not b -> b.Evaluate bitmap |> not
        | True -> true

type Bitmappings = Bitmapping array

let applyBitmapping (mappings : Bitmappings) (bitmap : Bitmap) : Bitmap =
    makeBitmap (seq {
        for k = 0 to (Array.length mappings - 1) do
            if mappings.[k].Evaluate(bitmap) then yield k
    })

let mirrorMapping keycount = Array.map (fun i -> Column (keycount - i - 1)) [|0 .. keycount - 1|]

let textMapping (string : string) : Bitmappings =
    string.ToCharArray() |>
    Array.map (fun (char : char) ->
        match char with
        | '-' -> Not True
        | _ -> (Char.GetNumericValue(char) |> int |> (fun x -> max 0 (x - 1)) |> Column)) 

let applyMappingBetween mapping time1 time2 (chart : Chart) : Chart =
    for (_, nr) in chart.Notes.EnumerateBetween time1 time2 do
        for i = 0 to 4 do nr.[i] <- applyBitmapping mapping nr.[i]
    chart

let mirror time1 time2 (chart : Chart) : Chart = applyMappingBetween (mirrorMapping (chart.Keys)) time1 time2 chart