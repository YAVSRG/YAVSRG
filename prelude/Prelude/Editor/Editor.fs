namespace Prelude.Editor

open System
open Prelude.Charts.Interlude

module Filter =

    type BitmapFunc =
        | Column of int
        | Not of BitmapFunc
        | True
        member this.Evaluate(bitmap : Bitmap) =
            match this with
            | Column i -> Bitmap.hasBit i bitmap
            | Not b -> b.Evaluate bitmap |> not
            | True -> true
    
    type Bitmapping = BitmapFunc array
    
    let applyBitmapping (mapping: Bitmapping) (bitmap: Bitmap) : Bitmap =
        Bitmap.create (seq {
            for k = 0 to (Array.length mapping - 1) do
                if mapping.[k].Evaluate(bitmap) then yield k
        })
    
    let mirrorMapping keycount = Array.init keycount (fun i -> Column (keycount - i - 1))
    
    let textMapping (string: string) : Bitmapping =
        string.ToCharArray() |>
        Array.map (fun (char: char) ->
            match char with
            | '-' -> Not True
            | _ -> Char.GetNumericValue char |> int |> (fun x -> max 0 (x - 1)) |> Column)
    
    let applyMappingBetween mapping time1 time2 (notes: TimeData<NoteRow>) =
        notes.MapBetween time1 time2
            (fun (_, nr) ->
                [|applyBitmapping mapping nr.[0]; applyBitmapping mapping nr.[1]; applyBitmapping mapping nr.[2]; applyBitmapping mapping nr.[3]; applyBitmapping mapping nr.[4];
                    nr.[5]; nr.[6]|])
        notes
    
    //todo: design a proper arrangement for filters on charts
    let mirror time1 time2 keys notes = applyMappingBetween (mirrorMapping keys) time1 time2 notes

