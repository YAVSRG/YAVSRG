namespace Prelude.Gameplay.Replays

open System
open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Mods

// "Replay" data is an array of timestamps + bitmaps of which keys are pressed down at that moment
// A new timestamp + bitmap is recorded when the state of one of the keys change; every timestamp corresponds to at least 1 key becoming pressed or unpressed
// Timestamps are relative to the first note in the chart; 0ms timestamp = +0ms relative to the first note
// This is so a replay is portable to a functionally identical copy of the chart other than its offset being shifted

// ex: [|(0, 0); (1, 1)|] indicates that the leftmost (1st) column was pressed down, 1 ms into the chart AKA 1ms after the first note

// ChartTime is conventionally used as the type signature instead of Time when the time is relative to the first note instead of to the start of the audio file
type ChartTime = float32<ms>

type ReplayRow = (struct (ChartTime * Bitmask))
type ReplayData = ReplayRow array

module Replay =

    let compress_to (output_stream: Stream) (data: ReplayData) =
        use gzip_stream = new GZipStream(output_stream, CompressionLevel.SmallestSize)
        use bw = new BinaryWriter(gzip_stream)

        bw.Write data.Length

        for (struct (time, buttons)) in data do
            bw.Write(float32 time)
            bw.Write buttons

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
            output.[i] <- struct (br.ReadSingle() * 1.0f<ms>, br.ReadUInt16())

        output

    let decompress_bytes (data: byte array) =
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
            let mutable last_time = Single.NegativeInfinity

            for i = 0 to (count - 1) do
                let time = br.ReadSingle()

                if not (Single.IsFinite time) then
                    failwith "replay contains invalid float value"

                if time < last_time then
                    failwithf "replay goes backwards in time %f -> %f" last_time time

                last_time <- time
                output.[i] <- struct (time * 1.0f<ms>, br.ReadUInt16())

            Ok output
        with err ->
            Error(err.ToString())

    let private perfect_replay_uncached (keys: int) (notes: TimeArray<NoteRow>) : ReplayData =
        let time_until_next i =
            if i >= notes.Length - 1 then
                50.0f<ms>
            else
                notes.[i + 1].Time - notes.[i].Time

        let first_note = notes.[0].Time

        seq {
            let mutable i = 0
            let mutable held: Bitmask = 0us

            while i < notes.Length do
                let { Time = time; Data = nr } = notes.[i]
                let delay = time_until_next i
                let mutable hit = held

                for k = 0 to (keys - 1) do
                    if nr.[k] = NoteType.NORMAL then
                        hit <- Bitmask.set_key k hit
                    elif nr.[k] = NoteType.HOLDHEAD then
                        hit <- Bitmask.set_key k hit
                        held <- Bitmask.set_key k held
                    elif nr.[k] = NoteType.HOLDTAIL then
                        hit <- Bitmask.unset_key k hit
                        held <- Bitmask.unset_key k held

                yield struct (time - first_note, hit)
                yield struct (time - first_note + delay * 0.5f, held)
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
        |> Array.map (fun struct (time, r) ->
            let new_time = max (last_time + 1.0f<ms>) (time + offset time)
            last_time <- new_time
            struct (new_time, r)
        )

    /// Generates a replay for provided notes where hits are slightly offset based on a sine wave over time
    /// Used in Interlude's HUD editor so you can see what varying hit ms deviations look like in various HUD elements
    let auto_replay_waving = auto_replay_waving_uncached |> cached