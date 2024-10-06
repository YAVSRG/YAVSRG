namespace Prelude.Tests.Charts

open Prelude
open Prelude.Charts

module ChartFuzzBuilder =

    let generate_notes (seed: int) (keys: int) (row_count: int) =
    
        let random = PseudoRandom.FromSeed seed

        let mutable last_time = 0.0f<ms>
        let mutable hold_notes = 0us
        let columns = { 0 .. keys - 1 } |> Array.ofSeq

        let next (_: int) : TimeItem<NoteRow> =
            let next_time = last_time + 1.0f<ms> + (random.Next 500 |> Time.of_number)
            let next_row = NoteRow.create_ln_bodies keys hold_notes

            let change_count_this_row = random.Next(keys) + 1
            random.Shuffle columns

            for i = 0 to change_count_this_row - 1 do
                let column = columns.[i]
                if Bitmask.has_key column hold_notes then
                    hold_notes <- Bitmask.unset_key column hold_notes
                    next_row.[column] <- NoteType.HOLDTAIL
                elif random.Next(2) = 1 then
                    hold_notes <- Bitmask.set_key column hold_notes
                    next_row.[column] <- NoteType.HOLDHEAD
                else
                    next_row.[column] <- NoteType.NORMAL

            last_time <- next_time

            { Time = next_time; Data = next_row }

        let notes = Array.init row_count next

        match TimeArray.last notes with
        | Some last_row ->
            for k = 0 to keys - 1 do
                if last_row.Data.[k] = NoteType.HOLDBODY then last_row.Data.[k] <- NoteType.HOLDTAIL
                elif last_row.Data.[k] = NoteType.HOLDHEAD then last_row.Data.[k] <- NoteType.NORMAL
        | None -> ()

        notes

    let generate_bpm_sv (seed: int) (points_count: int) =
        
        let sv = ResizeArray<TimeItem<float32>>()
        let bpm = ResizeArray<TimeItem<BPM>>()
        let random = PseudoRandom.FromSeed seed

        let mutable last_time = 0.0f<ms>

        bpm.Add({ Time = 0.0f<ms>; Data = { MsPerBeat = 120.0f<ms / beat>; Meter = 4<beat> }})

        for _ = 0 to points_count - 2 do
            let next_time = last_time + (random.Next 500 |> Time.of_number)
            match random.Next(3) with
            | 0 ->
                sv.Add({ Time = next_time; Data = random.Next(200) |> float32 |> ((*) 0.01f) })
                bpm.Add({ Time = next_time; Data = { MsPerBeat = 60000.0f<ms / minute> / (float32 (random.Next(240) + 1) * 1.0f<beat / minute>); Meter = 4<beat> } })
            | 1 ->
                sv.Add({ Time = next_time; Data = random.Next(200) |> float32 |> ((*) 0.01f) })
            | 2 -> 
                bpm.Add({ Time = next_time; Data = { MsPerBeat = 60000.0f<ms / minute> / (float32 (random.Next(240) + 1) * 1.0f<beat / minute>); Meter = 4<beat> } })
            | _ -> failwith "impossible"
            last_time <- next_time

        bpm.ToArray(), sv.ToArray()

    let generate (keys: int, seed: int) : Chart =
        let bpm, sv = generate_bpm_sv seed 100
        {
            Keys = keys
            BPM = bpm
            SV = sv
            Notes = generate_notes seed keys 100
        }