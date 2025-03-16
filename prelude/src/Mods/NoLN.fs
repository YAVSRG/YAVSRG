namespace Prelude.Mods

open Prelude
open Prelude.Charts

module NoLN =

    let compact_empty (data: TimeArray<NoteRow>) : TimeArray<NoteRow> =
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