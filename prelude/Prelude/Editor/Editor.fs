namespace Prelude.Editor

open System
open Prelude.Charts.Interlude

type ModChart = int * TimeData<NoteRow> * TimeData<BPM> * MultiTimeData<float> * string list

module Filter =

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
    
    let applyMappingBetween mapping time1 time2 (notes : TimeData<NoteRow>) =
        for (_, nr) in notes.EnumerateBetween time1 time2 do
            for i = 0 to 4 do nr.[i] <- applyBitmapping mapping nr.[i]
        notes
    
    //todo: design a proper arrangement for filters on charts
    let mirror time1 time2 keys notes = applyMappingBetween (mirrorMapping (keys)) time1 time2 notes

