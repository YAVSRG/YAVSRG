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
type [<Struct>] Replay =
    internal { Frames: ReplayFrame array }
    
    static member Empty : Replay = { Frames = [||] }
    
    member this.ToArray() : ReplayFrame array = this.Frames
    static member FromArray([<ParamArray>] frames: ReplayFrame array) : Replay = { Frames = frames }
    
    member this.WriteToStream(stream: Stream) : unit =
        let inline write_frames(frames: ReplayFrame array, bw: BinaryWriter) : unit =
            bw.Write(frames.Length)

            for replay_frame in frames do
                replay_frame.WriteToStream(bw)

            bw.Flush()
        
        use gzip_stream = new GZipStream(stream, CompressionLevel.SmallestSize)
        use bw = new BinaryWriter(gzip_stream)
        write_frames(this.Frames, bw)

    static member ReadFromStream(stream: Stream) : Replay =
        let inline read_frames(br: BinaryReader) : ReplayFrame array =
            let count: int = br.ReadInt32()
            let output = Array.zeroCreate count

            for i = 0 to count - 1 do
                output.[i] <- ReplayFrame.ReadFromStream(br)

            output
            
        use gzip_stream = new GZipStream(stream, CompressionMode.Decompress)
        use br = new BinaryReader(gzip_stream)
        { Frames = read_frames(br) }
        
    member inline this.ToByteArray() : byte array =
        use stream = new MemoryStream()
        this.WriteToStream(stream)
        stream.ToArray()
    
    static member inline FromByteArray(data: byte array) : Replay =
        use stream = new MemoryStream(data)
        Replay.ReadFromStream(stream)
        
    member inline this.ToBase64String() : string =
        Convert.ToBase64String(this.ToByteArray())
        
    static member inline FromBase64String(data: string) : Replay =
        Replay.FromByteArray(Convert.FromBase64String(data))
    

module Replay =

    let BYTES_PER_ROW = 6
    let MAX_ROWS_PER_SECOND = 200
    let MAX_BYTES_PER_SECOND = MAX_ROWS_PER_SECOND * BYTES_PER_ROW

    let decompress_string_untrusted (chart_duration: Time) (input: string) : Result<Replay, string> =
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

            Ok { Frames = output }
        with err ->
            Error(err.ToString())

    let private perfect_replay_uncached (keys: int) (notes: TimeArray<NoteRow>) : Replay =
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
        |> fun x -> { Frames = x }

    /// Generates perfect replay inputs for provided note data
    /// Should* score perfect judgements and accuracies on all notes and releases
    //  *unless you make a goofy ruleset where 0ms hits don't give 100% accuracy
    let perfect_replay = perfect_replay_uncached |> cached

    let private auto_replay_waving_uncached (keys: int) (notes: TimeArray<NoteRow>) : Replay =
        let mutable last_time = -Time.infinity

        let offset (time: Time) =
            MathF.Cos(time / 1000.0f<ms>) * 45.0f<ms>

        perfect_replay keys notes
        |> _.Frames
        |> Array.map (fun replay_frame ->
            let new_time = max (last_time + 1.0f<ms>) (replay_frame.Time + offset replay_frame.Time)
            last_time <- new_time
            { Time = new_time; PressedKeys = replay_frame.PressedKeys }
        )
        |> fun x -> { Frames = x }

    /// Generates a replay for provided notes where hits are slightly offset based on a sine wave over time
    /// Used in Interlude's HUD editor so you can see what varying hit ms deviations look like in various HUD elements
    let auto_replay_waving = auto_replay_waving_uncached |> cached