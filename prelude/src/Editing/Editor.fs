namespace Prelude.Editing

open System
open Prelude.Common
open Prelude.ChartFormats.Interlude

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
                printfn "        ~ %s" (NoteRow.prettyPrint d2)
                b <- b.Tail
            else
                printfn "%s ~ %s" (NoteRow.prettyPrint d1) (NoteRow.prettyPrint d2)
                a <- a.Tail
                b <- b.Tail
        for (_, d1) in a do
            printfn "%s ~ " (NoteRow.prettyPrint d1)
        for (_, d2) in b do
            printfn "        ~ %s" (NoteRow.prettyPrint d2)

    let apply keycount (bpms: TimeData<BPM>) (notes: TimeData<NoteRow>) : TimeData<NoteRow> =
        let mutable holding = Bitmap.empty
        let mutable last_yielded_holding = Bitmap.empty
        let mutable memory : NoteRow = NoteRow.createEmpty keycount
        let mutable memory_time : Time = -Time.infinity
        let mutable spacing = 0.0f<ms>

        let bpms = bpms.Data
        let mutable bpm_index = -1

        let update_spacing now = 
            if bpm_index + 1 < bpms.Count then
                let (time, (meter, msPerBeat)) = bpms.[bpm_index + 1]
                if time < now then
                    spacing <- msPerBeat * 0.5f<beat>
                    bpm_index <- bpm_index + 1

        seq {
            
            // set up first row
            let (time, d) = notes.First.Value
            memory_time <- time
            for k, nt in Array.indexed d do
                match nt with
                | NoteType.HOLDHEAD ->
                    memory.[k] <- NoteType.NORMAL
                | NoteType.NORMAL ->
                    memory.[k] <- NoteType.HOLDHEAD
                    holding <- Bitmap.setBit k holding
                | _ -> ()
            
            update_spacing time

            // for all other rows
            for (time, d) in Seq.skip 1 notes.Data do

                let currentRow = NoteRow.createEmpty keycount
                NoteRow.setNoteData NoteType.HOLDBODY currentRow holding
                let mutable check_for_releases = Bitmap.empty
                
                for k, nt in Array.indexed d do
                    match nt with
                    | NoteType.HOLDHEAD ->
                        currentRow.[k] <- NoteType.NORMAL
                        check_for_releases <- Bitmap.setBit k check_for_releases
                        holding <- Bitmap.unsetBit k holding
                    | NoteType.NORMAL ->
                        currentRow.[k] <- NoteType.HOLDHEAD
                        check_for_releases <- Bitmap.setBit k check_for_releases
                        holding <- Bitmap.setBit k holding
                    | _ -> ()

                let time_since_last_row = time - memory_time

                // RELEASES APPEAR IN LINE WITH PREVIOUS ROW
                if Time.Abs(time_since_last_row - spacing) < 5.0f<ms> then
                    for k in Bitmap.toSeq check_for_releases do
                        match memory.[k] with
                        | NoteType.HOLDHEAD -> memory.[k] <- NoteType.NORMAL
                        | NoteType.HOLDBODY -> memory.[k] <- NoteType.HOLDTAIL
                        | _ -> ()
                    yield (memory_time, memory)
                    //printfn "= %s" (NoteRow.prettyPrint memory)
                    last_yielded_holding <- NoteRow.noteData NoteType.HOLDBODY memory ||| NoteRow.noteData NoteType.HOLDHEAD memory

                // RELEASES APPEAR AFTER PREVIOUS ROW
                elif time_since_last_row > spacing then
                    yield (memory_time, memory)
                    //printfn "+ %s" (NoteRow.prettyPrint memory)
                    last_yielded_holding <- NoteRow.noteData NoteType.HOLDBODY memory ||| NoteRow.noteData NoteType.HOLDHEAD memory
                    let releases_row = NoteRow.createEmpty keycount
                    NoteRow.setNoteData NoteType.HOLDBODY releases_row last_yielded_holding
                    NoteRow.setNoteData NoteType.HOLDTAIL releases_row (last_yielded_holding &&& check_for_releases)
                    yield (time - spacing, releases_row)
                    last_yielded_holding <- NoteRow.noteData NoteType.HOLDBODY releases_row
                    //printfn "+ %s" (NoteRow.prettyPrint releases_row)

                // RELEASES APPEAR BEFORE PREVIOUS ROW
                else
                    let releases_row = NoteRow.createEmpty keycount
                    NoteRow.setNoteData NoteType.HOLDBODY releases_row last_yielded_holding
                    for k in Bitmap.toSeq check_for_releases do
                        if memory.[k] = NoteType.HOLDHEAD then memory.[k] <- NoteType.NORMAL
                        if Bitmap.hasBit k last_yielded_holding then
                            releases_row.[k] <- NoteType.HOLDTAIL
                            memory.[k] <- NoteType.NOTHING
                    yield (time - spacing, releases_row)
                    //printfn "- %s %.2f" (NoteRow.prettyPrint releases_row) (time - spacing)
                    yield (memory_time, memory)
                    //printfn "- %s %.2f" (NoteRow.prettyPrint memory) memory_time
                    last_yielded_holding <- NoteRow.noteData NoteType.HOLDBODY memory ||| NoteRow.noteData NoteType.HOLDHEAD memory
                    
                update_spacing time
                memory_time <- time
                memory <- currentRow

            // cut off last row
            for k, nt in Array.indexed memory do
                if nt = NoteType.HOLDBODY then memory.[k] <- NoteType.HOLDTAIL
                if nt = NoteType.HOLDHEAD then memory.[k] <- NoteType.NORMAL

            yield (memory_time, memory)

        }
        |> Seq.filter (snd >> NoteRow.isEmpty >> not)
        |> ResizeArray
        |> TimeData