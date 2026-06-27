namespace Prelude.Tests.Charts

open Prelude
open Prelude.Charts

type TimestampGenerator(random: PseudoRandom) =
    let mutable last_time = 0.0f<ms>

    member this.Next() =
        let next_time = last_time + 1.0f<ms> + Time.of_number(random.Next(500))
        last_time <- next_time
        next_time

type ChartFuzzer =

    static member GenerateNotes(seed: int, keys: int, row_count: int) : TimeArray<NoteRow> =
        let random = PseudoRandom.FromSeed(seed)
        let timestamps = TimestampGenerator(random)

        let columns = seq { 0 .. keys - 1 } |> Array.ofSeq

        let choose_random_columns() =
            let random_column_count = random.Next(keys) + 1
            random.Shuffle(columns)
            seq {
                for i = 0 to random_column_count - 1 do
                    yield columns.[i]
            }

        let mutable hold_notes = 0us
        let generate_next_row (_: int) : TimeItem<NoteRow> =

            let next_time = timestamps.Next()
            let next_row = NoteRow.create_ln_bodies keys hold_notes

            for column in choose_random_columns() do

                if Bitmask.has_key column hold_notes then
                    hold_notes <- Bitmask.unset_key column hold_notes
                    next_row.[column] <- NoteType.HOLDTAIL

                elif random.Next(2) = 1 then
                    hold_notes <- Bitmask.set_key column hold_notes
                    next_row.[column] <- NoteType.HOLDHEAD

                else
                    next_row.[column] <- NoteType.NORMAL

            { Time = next_time; Data = next_row }

        let end_unterminated_holds(notes: TimeArray<NoteRow>) =
            match TimeArray.last(notes) with
            | Some last_row ->
                for k = 0 to keys - 1 do
                    if last_row.Data.[k] = NoteType.HOLDBODY then last_row.Data.[k] <- NoteType.HOLDTAIL
                    elif last_row.Data.[k] = NoteType.HOLDHEAD then last_row.Data.[k] <- NoteType.NORMAL
            | None -> ()

        let notes = Array.init row_count generate_next_row
        end_unterminated_holds(notes)
        notes

    static member GenerateBPMAndSV(seed: int, points_count: int) : TimeArray<BPM> * TimeArray<SV> =
        let sv = ResizeArray<TimeItem<float32>>()
        let bpm = ResizeArray<TimeItem<BPM>>()
        let random = PseudoRandom.FromSeed(seed)
        let timestamps = TimestampGenerator(random)

        bpm.Add({ Time = 0.0f<ms>; Data = { MsPerBeat = 120.0f<ms / beat>; Meter = 4<beat> }})

        let random_sv(time: Time) =
            let speed =
                random.Next(200)
                |> float32
                |> ((*) 0.01f)
            { Time = time; Data = speed }

        let random_bpm(time: Time) =
            let random_bpm = float32 (random.Next(240) + 1) * 1.0f<beat / minute>
            let bpm_data = { MsPerBeat = MS_PER_MINUTE / random_bpm; Meter = 4<beat> }
            { Time = time; Data = bpm_data }

        for _ = 1 to points_count - 1 do
            let next_time = timestamps.Next()
            match random.Next(3) with
            | 0 ->
                sv.Add(random_sv(next_time))
                bpm.Add(random_bpm(next_time))
            | 1 ->
                sv.Add(random_sv(next_time))
            | 2 ->
                bpm.Add(random_bpm(next_time))
            | _ -> failwith "impossible"

        bpm.ToArray(), sv.ToArray()

    static member Generate(keys: int, seed: int) : Chart =
        let DEFAULT_SIZE = 100

        let bpm, sv = ChartFuzzer.GenerateBPMAndSV(seed, DEFAULT_SIZE)
        let notes = ChartFuzzer.GenerateNotes(seed, keys, DEFAULT_SIZE)

        {
            Keys = keys
            BPM = bpm
            SV = sv
            Notes = notes
        }