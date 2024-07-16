namespace Prelude.Charts.Processing

open System
open Prelude
open Prelude.Charts

(*
    Marker for status of mods.
        0 = This is a non-silly mod suitable for online upload, personal bests, leaderboards
        1 = This is a for-fun mod that may transform a chart in large ways or are otherwise not suitable for leaderboards e.g. randomiser
        2 = Scores with this mod enabled should not be saved at all e.g. something experimental or still in development
*)
type ModStatus =
    | Ranked = 0
    | Unranked = 1
    | Unstored = 2

type ModdedChart =
    {
        Keys: int
        Notes: TimeArray<NoteRow>
        BPM: TimeArray<BPM>
        SV: TimeArray<float32>
        ModsSelected: Map<string, int>
        ModsApplied: Map<string, int>
        Status: ModStatus
    }
    member this.FirstNote = this.Notes.[0].Time
    member this.LastNote = this.Notes.[this.Notes.Length - 1].Time

type ModdedChartInternal =
    {
        Keys: int
        Notes: TimeArray<NoteRow>
        BPM: TimeArray<BPM>
        SV: TimeArray<float32>
    }

module Mirror =

    let apply (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        { chart with
            Notes = TimeArray.map Array.rev chart.Notes
        },
        true

module NoSV =

    let apply (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        let mutable has_sv = false

        for { Data = s } in chart.SV do
            if MathF.Round(s, 2) <> 1.0f then
                has_sv <- true

        { chart with SV = [||] }, has_sv

module NoLN =

    let compact_empty (data: TimeArray<NoteRow>) =
        TimeArray.filter (NoteRow.is_empty >> not) data

    let apply (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        let mutable has_ln = false
        let notes_copy = TimeArray.map Array.copy chart.Notes

        for { Data = nr } in notes_copy do
            for k = 0 to chart.Keys - 1 do
                match nr.[k] with
                | NoteType.HOLDHEAD ->
                    has_ln <- true
                    nr.[k] <- NoteType.NORMAL
                | NoteType.HOLDBODY -> nr.[k] <- NoteType.NOTHING
                | NoteType.HOLDTAIL -> nr.[k] <- NoteType.NOTHING
                | _ -> ()

        { chart with Notes = notes_copy |> compact_empty }, has_ln

    let apply_shorter_than (length: float32<beat>) (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        let output = chart.Notes |> TimeArray.map Array.copy |> ResizeArray
        let mutable length_threshold = 0.0f<ms>
        let mutable bpm_index = -1

        let update_min_length now =
            while bpm_index < 0
                  || (bpm_index + 1 < chart.BPM.Length && chart.BPM.[bpm_index + 1].Time < now) do
                let ms_per_beat = chart.BPM.[bpm_index + 1].Data.MsPerBeat
                length_threshold <- ms_per_beat * length - 3.0f<ms>
                bpm_index <- bpm_index + 1

        let mutable short_hold_found = false

        let remove_short_hold k start_index =
            let mutable i = start_index + 1
            let start_time = output.[start_index].Time
            while output.[i].Data.[k] <> NoteType.HOLDTAIL && output.[i].Time - start_time < length_threshold do
                i <- i + 1

            if output.[i].Data.[k] = NoteType.HOLDTAIL then
                short_hold_found <- true
                for j = i downto start_index + 1 do
                    output.[j].Data.[k] <- NoteType.NOTHING
                output.[start_index].Data.[k] <- NoteType.NORMAL

        let mutable i = 0

        while i < output.Count do
            let { Time = time; Data = d } = output.[i]
            update_min_length time

            for k, nt in Array.indexed d do
                if nt = NoteType.HOLDHEAD then
                    remove_short_hold k i

            i <- i + 1

        { chart with
            Notes = output |> Array.ofSeq |> compact_empty
        },
        short_hold_found

module Inverse =

    // debug tool
    let diff (a: TimeArray<NoteRow>) (b: TimeArray<NoteRow>) =
        let mutable a = List.ofSeq a
        let mutable b = List.ofSeq b

        while a <> [] && b <> [] do
            let { Time = t1; Data = d1 } = a.Head
            let { Time = t2; Data = d2 } = b.Head

            if t1 < t2 then
                printfn "%s ~ " (NoteRow.pretty_print d1)
                a <- a.Tail
            elif t1 > t2 then
                printfn "     ~ %s" (NoteRow.pretty_print d2)
                b <- b.Tail
            else
                printfn "%s ~ %s" (NoteRow.pretty_print d1) (NoteRow.pretty_print d2)
                a <- a.Tail
                b <- b.Tail

        for { Data = d1 } in a do
            printfn "%s ~ " (NoteRow.pretty_print d1)

        for { Data = d2 } in b do
            printfn "     ~ %s" (NoteRow.pretty_print d2)

    let apply (gap_size: float32<beat>) (chart: ModdedChartInternal) : ModdedChartInternal * bool =

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
                else
                    row.[k] <- NoteType.NOTHING

                i <- i + 1

        let add_hold (k: int) (start_index: int) =

            if start_index = output.Count - 1 then
                ()
            else

                let { Time = start_time; Data = r } = output.[start_index]
                assert (r.[k] = NoteType.NORMAL)

                // fd
                let mutable found_next_note = false
                let mutable i = start_index + 1

                while i < output.Count && not found_next_note do
                    let { Data = row } = output.[i]

                    if row.[k] = NoteType.HOLDHEAD || row.[k] = NoteType.NORMAL then
                        found_next_note <- true

                    i <- i + 1

                if not found_next_note then // end of chart
                    assert (i = output.Count)
                    let { Data = row } = output.[i - 1] in
                    row.[k] <- NoteType.HOLDTAIL

                    for x = i - 2 downto start_index + 1 do
                        let { Data = row } = output.[x]
                        row.[k] <- NoteType.HOLDBODY

                    r.[k] <- NoteType.HOLDHEAD
                else
                    let intended_tail_time = output.[i - 1].Time - spacing

                    if intended_tail_time - spacing <= start_time then
                        ()
                    else

                        let mutable tail_position = i - 2 // index before found note

                        while output.[tail_position].Time > intended_tail_time do
                            tail_position <- tail_position - 1

                        assert (output.[tail_position].Time <= intended_tail_time)

                        let { Time = time; Data = row } = output.[tail_position]

                        if time = intended_tail_time then
                            row.[k] <- NoteType.HOLDTAIL
                        else
                            let new_row = NoteRow.create_empty chart.Keys

                            for key = 0 to chart.Keys - 1 do
                                if row.[key] = NoteType.HOLDBODY || row.[key] = NoteType.HOLDHEAD then
                                    new_row.[key] <- NoteType.HOLDBODY

                            new_row.[k] <- NoteType.HOLDTAIL
                            tail_position <- tail_position + 1

                            output.Insert(
                                tail_position,
                                {
                                    Time = intended_tail_time
                                    Data = new_row
                                }
                            )

                        for x = tail_position - 1 downto start_index + 1 do
                            let { Data = row } = output.[x]
                            row.[k] <- NoteType.HOLDBODY

                        r.[k] <- NoteType.HOLDHEAD

        let mutable bpm_index = -1

        let update_spacing now =
            while bpm_index < 0
                  || (bpm_index + 1 < chart.BPM.Length && chart.BPM.[bpm_index + 1].Time < now) do
                let msPerBeat = chart.BPM.[bpm_index + 1].Data.MsPerBeat
                spacing <- msPerBeat * gap_size
                bpm_index <- bpm_index + 1

        let mutable i = 0

        while i < output.Count do
            let { Time = time; Data = d } = output.[i]
            update_spacing time

            for k, nt in Array.indexed d do
                if nt = NoteType.NORMAL then
                    add_hold k i
                elif nt = NoteType.HOLDHEAD then
                    remove_hold k i

            i <- i + 1

        { chart with
            Notes = output |> Array.ofSeq
        },
        true

module MoreNotes =

    let apply_chordjacks (chart: ModdedChartInternal) : ModdedChartInternal * bool =

        if chart.Keys <> 4 then
            chart, false
        else

            let mutable previous_nr: NoteRow = Array.zeroCreate chart.Keys

            let add_note (if_jack_in_column: int) (add_to_column: int) (nr: NoteRow) =
                if
                    (nr.[if_jack_in_column] = NoteType.NORMAL
                     || nr.[if_jack_in_column] = NoteType.HOLDHEAD)
                    && previous_nr.[if_jack_in_column] = NoteType.NORMAL
                    && nr.[add_to_column] = NoteType.NOTHING
                then
                    nr.[add_to_column] <- NoteType.NORMAL

                nr

            let new_notes =
                chart.Notes
                |> TimeArray.map (fun nr ->
                    let new_notes =
                        nr
                        |> NoteRow.clone
                        |> add_note 0 1
                        |> add_note 1 0
                        |> add_note 2 3
                        |> add_note 3 2

                    previous_nr <- new_notes
                    new_notes
                )

            { chart with Notes = new_notes }, true

    let apply_minijacks (chart: ModdedChartInternal) : ModdedChartInternal * bool =

        if chart.Keys <> 4 then
            chart, false
        else

            let MS_THRESHOLD = 20.0f<ms>

            let mutable flip = false
            let mutable skip = true
            let mutable previous_nr: NoteRow = Array.zeroCreate chart.Keys
            let mutable previous_time: Time = Time.infinity

            let add_note (nr: NoteRow) =
                flip <- not flip

                match
                    if flip then
                        seq { 0 .. chart.Keys - 1 }
                    else
                        seq { chart.Keys - 1 .. -1 .. 0 }
                    |> Seq.tryFind (fun i -> previous_nr.[i] <> NoteType.NORMAL && nr.[i] = NoteType.NOTHING)
                with
                | Some i -> nr.[i] <- NoteType.NORMAL
                | None -> ()

                nr

            let new_notes =
                chart.Notes
                |> Array.map (fun { Time = time; Data = nr } ->
                    skip <- skip || (time - previous_time) < MS_THRESHOLD
                    let new_notes = nr |> NoteRow.clone |> (if not skip then add_note else id)

                    skip <- not skip
                    previous_nr <- new_notes
                    previous_time <- time
                    { Time = time; Data = new_notes }
                )

            { chart with Notes = new_notes }, true

module Randomise =
    
    let shuffle (seed: int) (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        let random = PseudoRandom.FromSeed(seed)
        let shuffled_columns = Array.init chart.Keys id 
        random.Shuffle shuffled_columns

        let shuffle_notes (nr: NoteRow) : NoteRow =
            let new_nr = NoteRow.create_empty chart.Keys
            for i = 0 to chart.Keys - 1 do
                new_nr.[i] <- nr.[shuffled_columns.[i]]
            new_nr

        { chart with Notes = TimeArray.map shuffle_notes chart.Notes }, true

    let randomise (seed: int) (chart: ModdedChartInternal) : ModdedChartInternal * bool =
        let random = PseudoRandom.FromSeed(seed)

        let original_chart_column_last_used = Array.create chart.Keys -Time.infinity

        let column_last_used = Array.create chart.Keys -Time.infinity
        let ln_column_map : int array = Array.zeroCreate chart.Keys

        let get_random_column (now: Time) (gap_size: Time) =
            let suitable_columns = 
                column_last_used
                |> Seq.indexed
                |> Seq.filter (fun (i, t) -> let time_ago = (now - t) in gap_size < time_ago * 1.3f && gap_size > time_ago * 0.7f )
                |> Seq.map fst
                |> Array.ofSeq
            if suitable_columns.Length > 0 then
                suitable_columns.[random.Next(suitable_columns.Length)]
            else

            let fallback_columns =
                column_last_used
                |> Seq.indexed
                |> Seq.filter (fun (i, t) -> t < now )
                |> Seq.sortBy snd
                |> Seq.map fst
                |> Array.ofSeq

            if fallback_columns.Length > 0 then
                fallback_columns.[0]

            else 

            failwith "impossible"

        let randomise_row (now: Time) (nr: NoteRow) : NoteRow =
            let new_row = NoteRow.create_empty chart.Keys
            for k = 0 to chart.Keys - 1 do
                if nr.[k] = NoteType.HOLDBODY then
                    new_row.[ln_column_map.[k]] <- NoteType.HOLDBODY
                elif nr.[k] = NoteType.HOLDTAIL then
                    new_row.[ln_column_map.[k]] <- NoteType.HOLDTAIL
                    column_last_used.[ln_column_map.[k]] <- now
                    original_chart_column_last_used.[k] <- now

            for k = 0 to chart.Keys - 1 do
                if nr.[k] = NoteType.HOLDHEAD then
                    let gap_size = now - original_chart_column_last_used.[k]
                    let col = get_random_column now gap_size
                    ln_column_map.[k] <- col
                    column_last_used.[col] <- Time.infinity
                    new_row.[col] <- NoteType.HOLDHEAD
                elif nr.[k] = NoteType.NORMAL then
                    let gap_size = now - original_chart_column_last_used.[k]
                    let col = get_random_column now gap_size
                    original_chart_column_last_used.[k] <- now
                    column_last_used.[col] <- now
                    new_row.[col] <- NoteType.NORMAL
            new_row

        { chart with 
            Notes = 
                chart.Notes
                |> Array.map (fun { Time = time; Data = nr } -> { Time = time; Data = randomise_row time nr })
        }, true