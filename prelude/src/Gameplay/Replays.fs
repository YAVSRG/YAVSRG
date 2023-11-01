namespace Prelude.Gameplay

open System
open System.IO
open System.IO.Compression
open Percyqaz.Common
open Prelude
open Prelude.Charts.Formats.Interlude

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

    let createDefault (missWindow: Time) (keys: int) (notes: TimeArray<NoteRow>) : InternalScoreData =
        notes
        |> Array.map (fun { Time = time; Data = nr } ->
            let times = Array.create keys missWindow
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

    // used for debug/test purposes, and not called normally.
    // creates what the internal data should look like after a "perfect" play
    let createAuto (keys: int) (notes: TimeArray<NoteRow>) : InternalScoreData =
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

    let decompress (data: string) : ReplayData =
        let compressed = Convert.FromBase64String data
        use inputStream = new MemoryStream(compressed)
        use gZipStream = new GZipStream(inputStream, CompressionMode.Decompress)
        use br = new BinaryReader(gZipStream)

        let count: int = br.ReadInt32()
        let output = Array.zeroCreate count

        for i = 0 to (count - 1) do
            output.[i] <- struct (br.ReadSingle() * 1.0f<ms>, br.ReadUInt16())

        output

    let compress (data: ReplayData) : string =
        use outputStream = new MemoryStream()
        use gZipStream = new GZipStream(outputStream, CompressionLevel.Optimal)
        use bw = new BinaryWriter(gZipStream)

        bw.Write data.Length

        for (struct (time, buttons)) in data do
            bw.Write(float32 time)
            bw.Write buttons

        bw.Flush()
        Convert.ToBase64String(outputStream.ToArray())

    // this replay is fed into score calculation when Auto-play is enabled
    let perfectReplay (keys: int) (notes: TimeArray<NoteRow>) : ReplayData =
        let timeUntilNext i =
            if i >= notes.Length - 1 then
                50.0f<ms>
            else
                notes.[i + 1].Time - notes.[i].Time

        let firstNote = notes.[0].Time

        seq {
            let mutable i = 0
            let mutable held: Bitmask = 0us

            while i < notes.Length do
                let { Time = time; Data = nr } = notes.[i]
                let delay = timeUntilNext i
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

                yield struct (time - firstNote, hit)
                yield struct (time - firstNote + delay * 0.5f, held)
                i <- i + 1
        }
        |> Array.ofSeq

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

    new(data: string) = StoredReplayProvider(Replay.decompress data)

    static member AutoPlay(keys, noteData) =
        Replay.perfectReplay keys noteData |> StoredReplayProvider

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

    let mutable currentState: Bitmask = 0us

    member this.PollReplay(time: ChartTime) =
        while replay.HasNext time do
            let struct (time, keystates) = replay.GetNext()
            this.HandleReplayRow(time, keystates)

    member this.HandleReplayRow(time, keystates) =
        for k = 0 to (keys - 1) do
            if Bitmask.has_key k currentState && not (Bitmask.has_key k keystates) then
                this.HandleKeyUp(time, k)
            elif Bitmask.has_key k keystates && not (Bitmask.has_key k currentState) then
                this.HandleKeyDown(time, k)

        currentState <- keystates

    member this.KeyState = currentState
    abstract member HandleKeyDown: ChartTime * int -> unit
    abstract member HandleKeyUp: ChartTime * int -> unit
