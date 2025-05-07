namespace Prelude.Gameplay.Replays

open Prelude

/// Abstraction that allows walking through a replay one row at a time
/// The underlying replay could be being created live as a player plays the chart, backed by some pre-existing score, or being streamed in from a network for multiplayer
type IReplay =
    /// Are we at the end of the replay?
    abstract member Finished: bool

    /// Is there a next bitmap before/on the given time?
    abstract member HasNext: ChartTime -> bool

    /// Get the next bitmap (call after checking HasNext = true)
    abstract member GetNext: unit -> ReplayFrame

    /// Get the underlying data (can throw if unavailable right now)
    abstract member GetFullReplay: unit -> ReplayData

    /// Walk backwards through recent inputs (for rendering input meter hud element)
    /// Sequence is only guaranteed to be valid for the frame it is used in, it should not be stored or iterated once the underlying replay has been modified
    abstract member EnumerateRecentFrames: unit -> ReplayFrame seq

/// Walks through a replay as far as data is available and triggers `HandleKeyDown` and `HandleKeyUp` as appropriate
[<AbstractClass>]
type ReplayConsumer(keys: int, replay: IReplay) =

    let mutable current_pressed_keys: Bitmask = 0us

    member this.PollReplay(time: ChartTime) =
        while replay.HasNext time do
            let struct (time, pressed_keys) = replay.GetNext()
            this.HandleReplayFrame(time, pressed_keys)

    member private this.HandleReplayFrame(time: ChartTime, pressed_keys: Bitmask) =
        for k = 0 to (keys - 1) do
            if Bitmask.has_key k current_pressed_keys && not (Bitmask.has_key k pressed_keys) then
                this.HandleKeyUp(time, k)
            elif Bitmask.has_key k pressed_keys && not (Bitmask.has_key k current_pressed_keys) then
                this.HandleKeyDown(time, k)

        current_pressed_keys <- pressed_keys

    member this.KeyState = current_pressed_keys
    abstract member HandleKeyDown: ChartTime * int -> unit
    abstract member HandleKeyUp: ChartTime * int -> unit