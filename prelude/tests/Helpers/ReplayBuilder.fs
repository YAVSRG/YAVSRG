namespace Prelude.Tests.Helpers

open Prelude
open Prelude.Gameplay.Replays

type ReplayBuilder() =
    let liveplay = LiveReplay(0.0f<ms>)

    let mutable state = 0us

    member this.KeyDown(time: Time, key: int) : ReplayBuilder =
        state <- state |> Bitmask.set_key key
        liveplay.AddFrame(time, state)
        this

    member this.KeyDown(time: Time) = this.KeyDown(time, 0)

    member this.KeyUp(time: Time, k: int) : ReplayBuilder =
        state <- state |> Bitmask.unset_key k
        liveplay.AddFrame(time, state)
        this

    member this.KeyUp(time: Time) = this.KeyUp(time, 0)

    member this.KeyDownUntil(time: Time, until: Time, key: int) = this.KeyDown(time, key).KeyUp(until, key)
    member this.KeyDownUntil(time: Time, until: Time) = this.KeyDownUntil(time, until, 0)

    member this.KeyDownFor(time: Time, duration: Time, key: int) = this.KeyDownUntil(time, time + duration, key)
    member this.KeyDownFor(time: Time, duration: Time) = this.KeyDownFor(time, duration, 0)

    member this.Build() : IReplay =
        liveplay.Finish()
        liveplay