namespace Prelude.Charts.Tools

open System
open Prelude.Common
open Prelude.Charts.Formats.Interlude

module Filter =

    type BitmapFunc =
        | Column of int
        | Not of BitmapFunc
        | True
        member this.Evaluate (bitmap: Bitmap) =
            match this with
            | Column i -> Bitmap.hasBit i bitmap
            | Not b -> b.Evaluate bitmap |> not
            | True -> true
    
    type Bitmapping = BitmapFunc array
    
    let applyBitmapping (mapping: Bitmapping) (bitmap: Bitmap) : Bitmap =
        Bitmap.create (seq {
            for k = 0 to (Array.length mapping - 1) do
                if mapping.[k].Evaluate bitmap then yield k
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
            ( fun (_, nr) ->
                let newr = NoteRow.createEmpty nr.Length
                let f noteType =
                    NoteRow.setNoteData noteType newr (applyBitmapping mapping (NoteRow.noteData noteType nr))
                f NoteType.NORMAL
                f NoteType.HOLDHEAD
                f NoteType.HOLDBODY
                f NoteType.HOLDTAIL
                newr
            )
        notes
    
    // todo: design a proper arrangement for filters on charts
    let mirror time1 time2 keys notes = applyMappingBetween (mirrorMapping keys) time1 time2 notes

module Inverse =

    let diff (a: TimeData<NoteRow>) (b: TimeData<NoteRow>) =
        let mutable a = List.ofSeq a.Data
        let mutable b = List.ofSeq b.Data

        while a <> [] && b <> [] do
            let (t1, d1) = a.Head
            let (t2, d2) = b.Head
            if t1 < t2 then
                printfn "%s ~ " (NoteRow.prettyPrint d1)
                a <- a.Tail
            elif t1 > t2 then
                printfn "     ~ %s" (NoteRow.prettyPrint d2)
                b <- b.Tail
            else
                printfn "%s ~ %s" (NoteRow.prettyPrint d1) (NoteRow.prettyPrint d2)
                a <- a.Tail
                b <- b.Tail
        for (_, d1) in a do
            printfn "%s ~ " (NoteRow.prettyPrint d1)
        for (_, d2) in b do
            printfn "     ~ %s" (NoteRow.prettyPrint d2)

    let apply (keycount: int) (bpms: TimeData<BPM>) (notes: TimeData<NoteRow>) : TimeData<NoteRow> =
        let bpms = bpms.Data
        let mutable bpm_index = -1
        let mutable spacing = 0.0f<ms>

        let output = ResizeArray<TimeDataItem<NoteRow>>()

        let update_spacing now = 
            while bpm_index + 1 < bpms.Count && fst bpms.[bpm_index + 1] < now do
                let msPerBeat = bpms.[bpm_index + 1] |> snd |> snd
                spacing <- msPerBeat * 0.25f<beat>
                bpm_index <- bpm_index + 1

        let insert_release k time =

            let mutable seek = output.Count - 1
            let mutable (rt, row) = output.[seek]

            let mutable place_tail = true

            while seek > 0 && rt > time do
                if row.[k] = NoteType.HOLDTAIL then 
                    row.[k] <- NoteType.NOTHING
                    place_tail <- false
                elif row.[k] = NoteType.HOLDHEAD then
                    row.[k] <- NoteType.NORMAL
                    place_tail <- false
                elif row.[k] = NoteType.NORMAL then
                    place_tail <- false

                seek <- seek - 1
                let a, b = output.[seek]
                rt <- a; row <- b

            if place_tail then

                if rt = time then row.[k] <- NoteType.HOLDTAIL
                else // rt < time
                    let r = NoteRow.createEmpty keycount
                    r.[k] <- NoteType.HOLDTAIL
                    output.Insert(seek + 1, (time, r))
        
        let mutable holding = Bitmap.empty

        // place skeleton of heads and tails
        for time, d in notes.Data do
            update_spacing time

            let new_row = NoteRow.createEmpty keycount
            for k, nt in Array.indexed d do
                match nt with
                | NoteType.HOLDHEAD ->
                    new_row.[k] <- NoteType.NORMAL
                    if Bitmap.hasBit k holding then insert_release k (time - spacing)
                    holding <- Bitmap.unsetBit k holding
                | NoteType.NORMAL ->
                    new_row.[k] <- NoteType.HOLDHEAD
                    if Bitmap.hasBit k holding then insert_release k (time - spacing)
                    holding <- Bitmap.setBit k holding
                | _ -> ()
            output.Add (time, new_row)

        // sever last row
        let last_row = snd output.[output.Count - 1]
        for k, nt in Array.indexed last_row do
            match nt with
            | NoteType.HOLDHEAD ->
                last_row.[k] <- NoteType.NORMAL
            | _ when Bitmap.hasBit k holding ->
                last_row.[k] <- NoteType.HOLDTAIL
            | _ -> ()

        // final pass
        let mutable holding = Bitmap.empty
        for _, d in output do
            for k, nt in Array.indexed d do
                match nt with
                | NoteType.HOLDHEAD ->
                    holding <- Bitmap.setBit k holding
                | NoteType.HOLDTAIL ->
                    holding <- Bitmap.unsetBit k holding
                | NoteType.NOTHING when Bitmap.hasBit k holding ->
                    d.[k] <- NoteType.HOLDBODY
                | _ -> ()

        TimeData output