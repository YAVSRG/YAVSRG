namespace Prelude.Gameplay.Replays

open System
open System.IO
open System.IO.Compression
open Prelude
open Prelude.Charts

// Unlike Time, ChartTime is relative to the first note of the chart, a value of 0 = exactly on the first note
// This is so a replay is portable to a functionally identical copy of the chart other than its offset being shifted
type ChartTime = float32<ms>

type [<Struct>] ReplayFrame =
    {
        Time: ChartTime
        PressedKeys: Bitmask
    }

    member inline this.WriteToStream(bw: BinaryWriter) : unit =
        bw.Write(float32 this.Time)
        bw.Write(this.PressedKeys.ToInt16())

    static member inline ReadFromStream(br: BinaryReader) : ReplayFrame =
        {
            Time = br.ReadSingle() * 1.0f<ms>
            PressedKeys = Bitmask.FromInt16(br.ReadUInt16())
        }

    static member inline Create(time: Time, pressed_keys: Bitmask) =
        { Time = time; PressedKeys = pressed_keys }

    static member inline Create(time: Time, pressed_keys: uint16) =
        ReplayFrame.Create(time, Bitmask.FromInt16(pressed_keys))

// Invariant: timestamps are nondecreasing
type ReplayData = ReplayFrame array

module Replay =

    let compress_to (output_stream: Stream) (replay: ReplayData) =
        use gzip_stream = new GZipStream(output_stream, CompressionLevel.SmallestSize)
        use bw = new BinaryWriter(gzip_stream)

        bw.Write(replay.Length)

        for replay_frame in replay do
            replay_frame.WriteToStream(bw)

        bw.Flush()

    let compress_bytes (data: ReplayData) : byte array =
        use stream = new MemoryStream()
        compress_to stream data
        stream.ToArray()

    let compress_string (data: ReplayData) : string =
        Convert.ToBase64String(compress_bytes data)

    let compressed_string_to_bytes (data: string) : byte array = Convert.FromBase64String data
    let compressed_bytes_to_string (data: byte array) : string = Convert.ToBase64String data

    let decompress_from (input_stream: Stream) : ReplayData =
        use gzip_stream = new GZipStream(input_stream, CompressionMode.Decompress)
        use br = new BinaryReader(gzip_stream)

        let count: int = br.ReadInt32()
        let output = Array.zeroCreate count

        for i = 0 to (count - 1) do
            output.[i] <- ReplayFrame.ReadFromStream(br)

        output

    let decompress_bytes (data: byte array) : ReplayData =
        use stream = new MemoryStream(data)
        decompress_from stream

    let decompress_string (data: string) : ReplayData =
        let bytes = Convert.FromBase64String data
        decompress_bytes bytes

    let BYTES_PER_ROW = 6
    let MAX_ROWS_PER_SECOND = 200
    let MAX_BYTES_PER_SECOND = MAX_ROWS_PER_SECOND * BYTES_PER_ROW

    let decompress_string_untrusted (chart_duration: Time) (input: string) : Result<ReplayData, string> =
        let max_rows = (10 + int (chart_duration / 1000.0f<ms>)) * MAX_ROWS_PER_SECOND

        try
            let bytes = Convert.FromBase64String input

            use input_stream = new MemoryStream(bytes)
            use gzip_stream = new GZipStream(input_stream, CompressionMode.Decompress)
            use br = new BinaryReader(gzip_stream)

            let count: int = br.ReadInt32()

            if count > max_rows then
                failwith "replay header indicates it is unreasonably big"

            let output = Array.zeroCreate count
            let mutable last_time = -Time.infinity

            for i = 0 to (count - 1) do
                let next_frame = ReplayFrame.ReadFromStream(br)

                if not (Single.IsFinite(float32 next_frame.Time)) then
                    failwith "replay contains invalid float value"

                if next_frame.Time < last_time then
                    failwithf "replay goes backwards in time %f -> %f" last_time next_frame.Time

                last_time <- next_frame.Time
                output.[i] <- next_frame

            Ok output
        with err ->
            Error(err.ToString())

    let private perfect_replay_uncached (keys: int) (notes: TimeArray<NoteRow>) : ReplayData =
        let time_until_next (current_index: int) : Time =
            if current_index + 1 >= notes.Length then
                100.0f<ms>
            else
                notes.[current_index + 1].Time - notes.[current_index].Time

        let first_note = notes.[0].Time

        seq {
            let mutable i = 0
            let mutable held: Bitmask = Bitmask.Empty

            while i < notes.Length do
                let { Time = time; Data = nr } = notes.[i]
                let delay = time_until_next(i) * 0.5f
                let mutable hit = held

                for k = 0 to (keys - 1) do
                    if nr.[k] = NoteType.NORMAL then
                        hit <- hit.Add(k)
                    elif nr.[k] = NoteType.HOLDHEAD then
                        hit <- hit.Add(k)
                        held <- held.Add(k)
                    elif nr.[k] = NoteType.HOLDTAIL then
                        hit <- hit.Remove(k)
                        held <- held.Remove(k)

                yield { Time = time - first_note; PressedKeys = hit }
                yield { Time = time - first_note + delay; PressedKeys = held }
                i <- i + 1
        }
        |> Array.ofSeq

    /// Generates perfect replay inputs for provided note data
    /// Should* score perfect judgements and accuracies on all notes and releases
    //  *unless you make a goofy ruleset where 0ms hits don't give 100% accuracy
    let perfect_replay = perfect_replay_uncached |> cached

    let private auto_replay_waving_uncached (keys: int) (notes: TimeArray<NoteRow>) : ReplayData =
        let mutable last_time = -Time.infinity

        let offset (time: Time) =
            MathF.Cos(time / 1000.0f<ms>) * 45.0f<ms>

        perfect_replay keys notes
        |> Array.map (fun replay_frame ->
            let new_time = max (last_time + 1.0f<ms>) (replay_frame.Time + offset replay_frame.Time)
            last_time <- new_time
            { Time = new_time; PressedKeys = replay_frame.PressedKeys }
        )

    /// Generates a replay for provided notes where hits are slightly offset based on a sine wave over time
    /// Used in Interlude's HUD editor so you can see what varying hit ms deviations look like in various HUD elements
    let auto_replay_waving = auto_replay_waving_uncached |> cached