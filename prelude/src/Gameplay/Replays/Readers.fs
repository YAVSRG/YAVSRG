namespace Prelude.Gameplay.Replays

open System.IO
open Percyqaz.Common
open Prelude
open Prelude.Charts

/// Abstraction that allows walking through a replay one row at a time
/// The underlying replay could be being created live as a player plays the chart, backed by some pre-existing score, or being streamed in from a network for multiplayer
// Concrete implementations below

type IReplayProvider =
    /// Are we at the end of the replay?
    abstract member Finished: bool

    /// Is there a next bitmap before/on the given time?
    abstract member HasNext: ChartTime -> bool

    /// Get the next bitmap (call after checking HasNext = true)
    abstract member GetNext: unit -> ReplayRow

    /// Get the underlying data (can throw if unavailable right now)
    abstract member GetFullReplay: unit -> ReplayData

    /// Walk backwards through recent inputs (for rendering input meter hud element)
    /// Sequence is only guaranteed to be valid for the frame it is used in, it should not be stored or iterated once the underlying replay has been modified
    abstract member EnumerateRecentEvents: unit -> ReplayRow seq

/// Replay provider for a replay that already existed beforehand
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

        member this.EnumerateRecentEvents() =
            seq {
                let mutable j = i - 1
                while j >= 0 do
                    yield data.[j]
                    j <- j - 1
            }

    new(data: string) = StoredReplayProvider(Replay.decompress_string data)

    static member AutoPlay(keys: int, notes: TimeArray<NoteRow>) : StoredReplayProvider =
        Replay.perfect_replay keys notes |> StoredReplayProvider

    static member WavingAutoPlay(keys: int, notes: TimeArray<NoteRow>) : StoredReplayProvider =
        Replay.auto_replay_waving keys notes |> StoredReplayProvider

/// Replay provider that supports writing information to the replay live during gameplay
type LiveReplayProvider(first_note: Time) =
    let mutable i = 0
    let mutable finished = false
    let mutable export = 0
    let buffer = ResizeArray<ReplayRow>()

    let mutable last_time : ChartTime = -Time.infinity

    member this.Add(time, bitmap) =
        if finished then invalidOp "Live play is declared as over; cannot append to replay"
        if time - first_note < last_time then
            failwithf "Timestamp for replay data went backwards: %f, %f" (time - first_note) last_time

        last_time <- time - first_note
        buffer.Add(struct (last_time, bitmap))

    member this.AddMultiplayerMarker(time, bitmap) =
        if finished then invalidOp "Live play is declared as over; cannot append to replay"
        if time - first_note >= last_time then
            last_time <- time - first_note
            buffer.Add(struct (last_time, bitmap))

        last_time <- time - first_note
        buffer.Add(struct (last_time, bitmap))

    interface IReplayProvider with
        member this.Finished = finished

        member this.HasNext (time: ChartTime) =
            if i >= buffer.Count then
                false
            else
                let struct (t, _) = buffer.[i]
                t <= time

        member this.GetNext() =
            i <- i + 1
            buffer.[i - 1]

        member this.GetFullReplay() =
            if not finished then invalidOp "Live play is not declared as over, we don't have the full replay yet!"
            buffer.ToArray()

        member this.EnumerateRecentEvents() =
            seq {
                let mutable j = buffer.Count - 1
                while j >= 0 do
                    yield buffer.[j]
                    j <- j - 1
            }

    member this.Finish() =
        if finished then invalidOp "Live play is already declared as over; cannot do so again"
        finished <- true

    /// Export all data not yet exported, for sending over a network in multiplayer
    member this.ExportLiveBlock(bw: BinaryWriter) =
        while export < buffer.Count do
            let struct (time, bitmap) = buffer.[export]

            if time > -1000.0f<ms> then
                bw.Write(float32 time)
                bw.Write bitmap

            export <- export + 1

        bw.Flush()

/// Replay provider that represents another player's score, streaming in from a network
type OnlineReplayProvider() =
    let mutable i = 0
    let mutable finished = false
    let mutable current_chart_time = 0.0f<ms>
    let buffer = ResizeArray<ReplayRow>()

    interface IReplayProvider with
        member this.Finished = finished

        member this.HasNext (time: ChartTime) =
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

        member this.EnumerateRecentEvents() =
            seq {
                let mutable j = i - 1
                while j >= 0 do
                    yield buffer.[j]
                    j <- j - 1
            }

    member this.ImportLiveBlock(timestamp: float32<ms>, br: BinaryReader) =
        if finished then
            invalidOp "Online play is declared as over; cannot append to replay"
        elif timestamp < current_chart_time then
            failwithf "Timestamp for replay data went backwards: %f, %f" timestamp current_chart_time
        else
            current_chart_time <- timestamp
            try
                while not (br.BaseStream.Position = br.BaseStream.Length) do
                    buffer.Add(struct (br.ReadSingle() * 1.0f<ms>, br.ReadUInt16()))
            with err ->
                Logging.Error "Error while receiving online replay data: %O" err

    member this.Finish() =
        if not finished then
            finished <- true
        else
            invalidOp "Online play is already declared as over; cannot do so again"

    member this.Time() : ChartTime = current_chart_time

/// Walks through a replay provider as far as data is available and triggers `HandleKeyDown` and `HandleKeyUp` as appropriate
[<AbstractClass>]
type ReplayConsumer(keys: int, replay: IReplayProvider) =

    let mutable current_pressed_keys: Bitmask = 0us

    member this.PollReplay(time: ChartTime) =
        while replay.HasNext time do
            let struct (time, keystates) = replay.GetNext()
            this.HandleReplayRow(time, keystates)

    member private this.HandleReplayRow(time, keystates) =
        for k = 0 to (keys - 1) do
            if Bitmask.has_key k current_pressed_keys && not (Bitmask.has_key k keystates) then
                this.HandleKeyUp(time, k)
            elif Bitmask.has_key k keystates && not (Bitmask.has_key k current_pressed_keys) then
                this.HandleKeyDown(time, k)

        current_pressed_keys <- keystates

    member this.KeyState = current_pressed_keys
    abstract member HandleKeyDown: ChartTime * int -> unit
    abstract member HandleKeyUp: ChartTime * int -> unit