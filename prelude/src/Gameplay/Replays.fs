﻿namespace Prelude.Gameplay

open System
open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude
open Prelude.Charts

// internally, a score is made up of flags on what a player has to do here

type HitStatus =
    | NOTHING = 0

    | HIT_REQUIRED = 1
    | HIT_HOLD_REQUIRED = 2
    | HIT_ACCEPTED = 3

    | RELEASE_REQUIRED = 4
    | RELEASE_ACCEPTED = 5

// this data is in-memory only and not exposed much to other parts of the code
// the flags in particular need never be exposed anywhere else, while the hit deltas can be used on the score screen to give useful data
// most interesting things should come out of a specific score system implementation

type InternalScoreDataRow = (struct (Time * Time array * HitStatus array))
type InternalScoreData = InternalScoreDataRow array

module InternalScore =

    let inline offsetOf (struct (t, _, _): InternalScoreDataRow) = t

    let create_gameplay (miss_window: Time) (keys: int) (notes: TimeArray<NoteRow>) : InternalScoreData =
        notes
        |> Array.map (fun { Time = time; Data = nr } ->
            let times = Array.create keys miss_window
            let statuses = Array.create keys HitStatus.NOTHING

            for k = 0 to (keys - 1) do
                if nr.[k] = NoteType.NORMAL then
                    statuses.[k] <- HitStatus.HIT_REQUIRED
                elif nr.[k] = NoteType.HOLDHEAD then
                    statuses.[k] <- HitStatus.HIT_HOLD_REQUIRED
                elif nr.[k] = NoteType.HOLDTAIL then
                    statuses.[k] <- HitStatus.RELEASE_REQUIRED

            struct (time, times, statuses)
        )

    // used for debugging and test rigs as what the internal data should look like after a "perfect" play
    let create_autoplay (keys: int) (notes: TimeArray<NoteRow>) : InternalScoreData =
        notes
        |> Array.map (fun { Time = time; Data = nr } ->
            let times = Array.zeroCreate keys
            let statuses = Array.create keys HitStatus.NOTHING

            for k = 0 to (keys - 1) do
                if nr.[k] = NoteType.NORMAL || nr.[k] = NoteType.HOLDHEAD then
                    statuses.[k] <- HitStatus.HIT_ACCEPTED
                elif nr.[k] = NoteType.HOLDTAIL then
                    statuses.[k] <- HitStatus.RELEASE_ACCEPTED

            struct (time, times, statuses)
        )

// "replay" data in the sense that, with this data + the chart it was played against, you can know everything about the score and what happened

// the Time is the ms offset into the chart*, the Bitmap is a map of which keys are pressed down at that moment
// ex: [|(0, 0); (1, 1)|] indicates that the leftmost (1st) column was pressed down, 1 ms into the chart
// *into the chart = time since the first note, not audio time

type ChartTime = float32<ms> //Indicates that 0 = first note
type ReplayRow = (struct (ChartTime * Bitmask))
type ReplayData = ReplayRow array

module Replay =

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

    let compress_to (output_stream: Stream) (data: ReplayData) =
        use gzip_stream = new GZipStream(output_stream, CompressionLevel.Optimal)
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

    let compressed_string_to_bytes (data: string) = Convert.FromBase64String data
    let compressed_bytes_to_string (data: byte array) = Convert.ToBase64String data

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

                if Single.IsNaN time || Single.IsInfinity time then
                    failwith "replay contains invalid float value"

                if time < last_time then
                    failwithf "replay goes backwards in time %f -> %f" last_time time

                last_time <- time
                output.[i] <- struct (time * 1.0f<ms>, br.ReadUInt16())

            Ok output
        with err ->
            Error(err.ToString())

    // the replay generated when Auto-play is enabled
    let perfect_replay (keys: int) (notes: TimeArray<NoteRow>) : ReplayData =
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

    let auto_replay_waving (keys: int) (notes: TimeArray<NoteRow>) : ReplayData =
        let mutable last_time = -Time.infinity

        let offset (time: Time) =
            MathF.Cos(time / 1000.0f<ms>) * 45.0f<ms>

        perfect_replay keys notes
        |> Array.map (fun struct (time, r) ->
            let new_time = max (last_time + 1.0f<ms>) (time + offset time)
            last_time <- new_time
            struct (new_time, r)
        )

type IReplayProvider =
    // are we at the end of the replay?
    abstract member Finished: bool

    // is there a next bitmap before/on the given time?
    abstract member HasNext: ChartTime -> bool

    // get the next bitmap (call if HasNext was true)
    abstract member GetNext: unit -> ReplayRow

    // get the underlying data
    abstract member GetFullReplay: unit -> ReplayData

type StoredReplayProvider(data: ReplayData) =
    let mutable i = 0

    interface IReplayProvider with
        member this.Finished = i >= data.Length

        member this.HasNext(time) =
            if i >= data.Length then
                false
            else
                let struct (t, _) = data.[i]
                t <= time

        member this.GetNext() =
            i <- i + 1
            data.[i - 1]

        member this.GetFullReplay() = data

    new(data: string) = StoredReplayProvider(Replay.decompress_string data)

    static member AutoPlay(keys, notes) =
        Replay.perfect_replay keys notes |> StoredReplayProvider

    static member WavingAutoPlay(keys, notes) =
        Replay.auto_replay_waving keys notes |> StoredReplayProvider

type LiveReplayProvider(firstNote: Time) =
    let mutable i = 0
    let mutable finished = false
    let mutable export = 0
    let buffer = ResizeArray<ReplayRow>()

    interface IReplayProvider with
        member this.Finished = finished

        member this.HasNext time =
            if i >= buffer.Count then
                false
            else
                let struct (t, _) = buffer.[i]
                t <= time

        member this.GetNext() =
            i <- i + 1
            buffer.[i - 1]

        member this.GetFullReplay() =
            if finished then
                buffer.ToArray()
            else
                invalidOp "Live play is not declared as over, we don't have the full replay yet!"

    member this.Add(time, bitmap) =
        if not finished then
            buffer.Add(struct (time - firstNote, bitmap))
        else
            invalidOp "Live play is declared as over; cannot append to replay"

    member this.Finish() =
        if not finished then
            finished <- true
        else
            invalidOp "Live play is already declared as over; cannot do so again"

    member this.ExportLiveBlock(bw: BinaryWriter) =
        while export < buffer.Count do
            let struct (time, bitmap) = buffer.[export]

            if time > -1000.0f<ms> then
                bw.Write(float32 time)
                bw.Write bitmap

            export <- export + 1

        bw.Flush()

type OnlineReplayProvider() =
    let mutable i = 0
    let mutable finished = false
    let mutable current_chart_time = 0.0f<ms>
    let buffer = ResizeArray<ReplayRow>()

    interface IReplayProvider with
        member this.Finished = finished

        member this.HasNext time =
            if i >= buffer.Count then
                false
            else
                let struct (t, _) = buffer.[i]
                t <= time

        member this.GetNext() =
            i <- i + 1
            buffer.[i - 1]

        member this.GetFullReplay() =
            if finished then
                buffer.ToArray()
            else
                invalidOp "Online play is not declared as over, we don't have the full replay yet!"

    member this.ImportLiveBlock(br: BinaryReader) =
        if finished then
            invalidOp "Online play is declared as over; cannot append to replay"
        else

            try
                while not (br.BaseStream.Position = br.BaseStream.Length) do
                    let t = br.ReadSingle() * 1.0f<ms>
                    buffer.Add(struct (t, br.ReadUInt16()))
                    current_chart_time <- t
            with err ->
                Logging.Error("Error while receiving online replay data", err)

    member this.Finish() =
        if not finished then
            finished <- true
        else
            invalidOp "Online play is already declared as over; cannot do so again"

    member this.Time() : ChartTime = current_chart_time

// provides an interface to read keypresses out of a replay easily
[<AbstractClass>]
type ReplayConsumer(keys: int, replay: IReplayProvider) =

    let mutable current_pressed_keys: Bitmask = 0us

    member this.PollReplay(time: ChartTime) =
        while replay.HasNext time do
            let struct (time, keystates) = replay.GetNext()
            this.HandleReplayRow(time, keystates)

    member this.HandleReplayRow(time, keystates) =
        for k = 0 to (keys - 1) do
            if Bitmask.has_key k current_pressed_keys && not (Bitmask.has_key k keystates) then
                this.HandleKeyUp(time, k)
            elif Bitmask.has_key k keystates && not (Bitmask.has_key k current_pressed_keys) then
                this.HandleKeyDown(time, k)

        current_pressed_keys <- keystates

    member this.KeyState = current_pressed_keys
    abstract member HandleKeyDown: ChartTime * int -> unit
    abstract member HandleKeyUp: ChartTime * int -> unit
