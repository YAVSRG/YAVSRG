namespace Prelude.Charts.Tools

open System
open Prelude
open Prelude.Charts.Formats.Interlude

type ModChart =
    {
        Keys: int
        Notes: TimeArray<NoteRow>
        BPM: TimeArray<BPM>
        SV: TimeArray<float32>
        ColumnSV: TimeArray<float32> array
        ModsUsed: string list
    }

module ModChart =
    
    let ofChart (chart: Chart) =
        {
            Keys = chart.Keys
            Notes = chart.Notes
            BPM = chart.BPM
            SV = chart.SV
            ColumnSV = chart.ColumnSV
            ModsUsed = []
        }

module Mirror =
    
    let apply (chart: ModChart) : ModChart * bool = 
        { chart with Notes = TimeArray.map Array.rev chart.Notes; ColumnSV = Array.rev chart.ColumnSV }, true

module NoSV =

    let apply (chart: ModChart) : ModChart * bool =
        let mutable has_sv = false
        for { Data = s } in chart.SV do
            if MathF.Round(s, 2) <> 1.0f then has_sv <- true
        for sv in chart.ColumnSV do
            for { Data = s } in sv do
                if MathF.Round(s, 2) <> 1.0f then has_sv <- true
        { chart with SV = [||]; ColumnSV = Array.init chart.Keys (fun _ -> [||]) }, has_sv

module NoLN =

    let apply (chart: ModChart) : ModChart * bool =
        let mutable has_ln = false
        let notes_copy = TimeArray.map Array.copy chart.Notes
        for { Data = nr } in notes_copy do
            for k = 0 to chart.Keys - 1 do
                match nr.[k] with
                | NoteType.HOLDHEAD -> has_ln <- true; nr.[k] <- NoteType.NORMAL
                | NoteType.HOLDBODY -> nr.[k] <- NoteType.NOTHING
                | NoteType.HOLDTAIL -> nr.[k] <- NoteType.NOTHING
                | _ -> ()
        { chart with Notes = notes_copy }, has_ln

module Inverse =

    // debug tool
    let diff (a: TimeArray<NoteRow>) (b: TimeArray<NoteRow>) =
        let mutable a = List.ofSeq a
        let mutable b = List.ofSeq b

        while a <> [] && b <> [] do
            let { Time = t1; Data = d1 } = a.Head
            let { Time = t2; Data = d2 } = b.Head
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
        for { Data = d1 } in a do
            printfn "%s ~ " (NoteRow.prettyPrint d1)
        for { Data = d2 } in b do
            printfn "     ~ %s" (NoteRow.prettyPrint d2)

    let apply (halved: bool) (chart: ModChart) : ModChart * bool =
        
        let output = chart.Notes |> TimeArray.map Array.copy |> ResizeArray

        let mutable spacing = 0.0f<ms>

        let remove_hold (k: int) (start_index: int) =
            let { Data = r } = output.[start_index] 
            assert (r.[k] = NoteType.HOLDHEAD)
            r.[k] <- NoteType.NORMAL

            let mutable finished = false
            let mutable i = start_index + 1
            while not finished do
                let { Data = row } = output.[i]
                if row.[k] = NoteType.HOLDTAIL then
                    row.[k] <- NoteType.NOTHING
                    finished <- true
                else row.[k] <- NoteType.NOTHING
                i <- i + 1

        let add_hold (k: int) (start_index: int) =

            if start_index = output.Count - 1 then () else

            let { Time = start_time; Data = r } = output.[start_index]
            assert (r.[k] = NoteType.NORMAL)

            // fd
            let mutable found_next_note = false
            let mutable i = start_index + 1
            while i < output.Count && not found_next_note do
                let { Data = row } = output.[i]
                if row.[k] = NoteType.HOLDHEAD || row.[k] = NoteType.NORMAL then found_next_note <- true
                i <- i + 1

            if not found_next_note then // end of chart
                assert (i = output.Count)
                let { Data = row } = output.[i - 1] in row.[k] <- NoteType.HOLDTAIL
                for x = i - 2 downto start_index + 1 do
                    let { Data = row } = output.[x]
                    row.[k] <- NoteType.HOLDBODY
                r.[k] <- NoteType.HOLDHEAD
            else
                let intended_tail_time = output.[i - 1].Time - spacing

                if intended_tail_time - spacing <= start_time then () else

                let mutable tail_position = i - 2 // index before found note
                while output.[tail_position].Time > intended_tail_time do
                    tail_position <- tail_position - 1

                assert(output.[tail_position].Time <= intended_tail_time)

                let { Time = time; Data = row } = output.[tail_position]
                if time = intended_tail_time then
                    row.[k] <- NoteType.HOLDTAIL
                else
                    let new_row = NoteRow.createEmpty chart.Keys
                    for key = 0 to chart.Keys - 1 do
                        if row.[key] = NoteType.HOLDBODY || row.[key] = NoteType.HOLDHEAD then new_row.[key] <- NoteType.HOLDBODY
                    new_row.[k] <- NoteType.HOLDTAIL
                    tail_position <- tail_position + 1
                    output.Insert(tail_position, { Time = intended_tail_time; Data = new_row })

                for x = tail_position - 1 downto start_index + 1 do
                    let { Data = row } = output.[x]
                    row.[k] <- NoteType.HOLDBODY

                r.[k] <- NoteType.HOLDHEAD

        let mutable bpm_index = -1

        let update_spacing now = 
            while bpm_index < 0 || (bpm_index + 1 < chart.BPM.Length && chart.BPM.[bpm_index + 1].Time < now) do
                let msPerBeat = chart.BPM.[bpm_index + 1].Data.MsPerBeat
                spacing <- msPerBeat * if halved then 0.125f<beat> else 0.25f<beat>
                bpm_index <- bpm_index + 1

        let mutable i = 0
        while i < output.Count do
            let { Time = time; Data = d } = output.[i]
            update_spacing time

            for k, nt in Array.indexed d do
                if nt = NoteType.NORMAL then add_hold k i
                elif nt = NoteType.HOLDHEAD then remove_hold k i

            i <- i + 1

        { chart with Notes = output |> Array.ofSeq }, true