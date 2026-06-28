namespace Prelude.Gameplay.Replays

open System
open System.IO
open System.IO.Compression
open Prelude

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

    static member inline Create(time: Time, pressed_keys: Bitmask) : ReplayFrame =
        { Time = time; PressedKeys = pressed_keys }

    static member inline Create(time: Time, pressed_keys: uint16) : ReplayFrame =
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
    
    static member FromUntrustedBase64String(chart_duration: Time, data: string) : Result<Replay, string> =
        let MAX_ROWS_PER_SECOND = 200
        let EXTRA_SLACK_SECONDS = 10
        
        let mutable last_time = -Time.infinity
        let inline check_frame(frame: ReplayFrame) : unit =
            if not (Single.IsFinite(float32 frame.Time)) then
                failwith "replay contains invalid float value"

            if frame.Time < last_time then
                failwithf "replay goes backwards in time %f -> %f" last_time frame.Time
                
            last_time <- frame.Time
        
        let inline read_frames_checked(br: BinaryReader) : ReplayFrame array =
            let max_rows = (EXTRA_SLACK_SECONDS + int (chart_duration / 1000.0f<ms>)) * MAX_ROWS_PER_SECOND

            let count: int = br.ReadInt32()
            if count > max_rows then
                failwith "replay header indicates it is unreasonably big"

            let output = Array.zeroCreate count

            for i = 0 to (count - 1) do
                let next_frame = ReplayFrame.ReadFromStream(br)
                check_frame(next_frame)
                output.[i] <- next_frame
                
            output
                
        try
            let replay_bytes = Convert.FromBase64String(data)

            use input_stream = new MemoryStream(replay_bytes)
            use gzip_stream = new GZipStream(input_stream, CompressionMode.Decompress)
            use br = new BinaryReader(gzip_stream)
            
            let frames = read_frames_checked(br)
            Ok { Frames = frames }
        with err ->
            Error(err.ToString())