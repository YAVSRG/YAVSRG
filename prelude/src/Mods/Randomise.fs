namespace Prelude.Mods

open Prelude
open Prelude.Charts

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