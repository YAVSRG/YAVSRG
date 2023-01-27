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

    let no_sv (keys: int) (sv: MultiTimeData<float32>) : MultiTimeData<float32> * bool =
        let mutable has_sv = false
        for i = 0 to keys do
            for (_, v) in (sv.GetChannelData (i - 1)).Data do
                if MathF.Round(v, 2) <> 1.0f then has_sv <- true
        MultiTimeData keys, has_sv


module Inverse =

    // debug tool
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

    let apply (keycount: int) (bpms: TimeData<BPM>) (notes: TimeData<NoteRow>) (halved: bool) : TimeData<NoteRow> =
        
        let output = notes.Data |> Seq.map (fun (t, xs) -> (t, Array.copy xs)) |> ResizeArray

        let mutable spacing = 0.0f<ms>

        let remove_hold (k: int) (start_index: int) =
            let _, r = output.[start_index]
            assert (r.[k] = NoteType.HOLDHEAD)
            r.[k] <- NoteType.NORMAL

            let mutable finished = false
            let mutable i = start_index + 1
            while not finished do
                let _, row = output.[i]
                if row.[k] = NoteType.HOLDTAIL then
                    row.[k] <- NoteType.NOTHING
                    finished <- true
                else row.[k] <- NoteType.NOTHING
                i <- i + 1

        let add_hold (k: int) (start_index: int) =

            if start_index = output.Count - 1 then () else

            let start_time, r = output.[start_index]
            assert (r.[k] = NoteType.NORMAL)

            // fd
            let mutable found_next_note = false
            let mutable i = start_index + 1
            while i < output.Count && not found_next_note do
                let _, row = output.[i]
                if row.[k] = NoteType.HOLDHEAD || row.[k] = NoteType.NORMAL then found_next_note <- true
                i <- i + 1

            if not found_next_note then // end of chart
                assert (i = output.Count)
                let _, row = output.[i - 1] in row.[k] <- NoteType.HOLDTAIL
                for x = i - 2 downto start_index + 1 do
                    let _, row = output.[x]
                    row.[k] <- NoteType.HOLDBODY
                r.[k] <- NoteType.HOLDHEAD
            else
                let intended_tail_time = fst output.[i - 1] - spacing

                if intended_tail_time - spacing <= start_time then () else

                let mutable tail_position = i - 2 // index before found note
                while (let time, _ = output.[tail_position] in time > intended_tail_time) do
                    tail_position <- tail_position - 1

                assert(fst output.[tail_position] <= intended_tail_time)

                let time, row = output.[tail_position]
                if (fst output.[tail_position] = intended_tail_time) then
                    row.[k] <- NoteType.HOLDTAIL
                else
                    let new_row = NoteRow.createEmpty keycount
                    for key = 0 to keycount - 1 do
                        if row.[key] = NoteType.HOLDBODY || row.[key] = NoteType.HOLDHEAD then new_row.[key] <- NoteType.HOLDBODY
                    new_row.[k] <- NoteType.HOLDTAIL
                    tail_position <- tail_position + 1
                    output.Insert(tail_position, (intended_tail_time, new_row))

                for x = tail_position - 1 downto start_index + 1 do
                    let _, row = output.[x]
                    row.[k] <- NoteType.HOLDBODY

                r.[k] <- NoteType.HOLDHEAD

        let bpms = bpms.Data
        let mutable bpm_index = -1

        let update_spacing now = 
            while bpm_index < 0 || (bpm_index + 1 < bpms.Count && fst bpms.[bpm_index + 1] < now) do
                let msPerBeat = bpms.[bpm_index + 1] |> snd |> snd
                spacing <- msPerBeat * if halved then 0.125f<beat> else 0.25f<beat>
                bpm_index <- bpm_index + 1

        let mutable i = 0
        while i < output.Count do
            let time, d = output.[i]
            update_spacing time

            for k, nt in Array.indexed d do
                if nt = NoteType.NORMAL then add_hold k i
                elif nt = NoteType.HOLDHEAD then remove_hold k i

            i <- i + 1

        TimeData output