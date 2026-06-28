namespace Prelude.Gameplay.Replays

open Prelude

/// Walks through a replay as far as data is available and triggers `HandleKeyDown` and `HandleKeyUp` as appropriate
[<AbstractClass>]
type KeyPressReader(keys: int, replay: ReplaySource) =

    let mutable current_pressed_keys: Bitmask = Bitmask.Empty

    member this.PollReplay(time: ChartTime) : unit =
        while replay.HasNext(time) do
            let replay_frame = replay.GetNext()
            this.HandleReplayFrame(replay_frame.Time, replay_frame.PressedKeys)

    member private this.HandleReplayFrame(time: ChartTime, new_pressed_keys: Bitmask) : unit =
        for k = 0 to keys - 1 do

            if current_pressed_keys.Contains(k) && not (new_pressed_keys.Contains(k)) then
                this.HandleKeyUp(time, k)

            elif new_pressed_keys.Contains(k) && not (current_pressed_keys.Contains(k)) then
                this.HandleKeyDown(time, k)

        current_pressed_keys <- new_pressed_keys

    member this.KeyState = current_pressed_keys
    abstract member HandleKeyDown: ChartTime * int -> unit
    abstract member HandleKeyUp: ChartTime * int -> unit