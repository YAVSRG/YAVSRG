namespace Prelude.Mods

open Prelude
open Prelude.Charts

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