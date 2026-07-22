namespace Prelude.Tests.Helpers

open System
open Prelude
open Prelude.Gameplay.Replays

type ReplayBuilder() =
    let liveplay = GameplayReplaySource(0.0f<ms>)

    let mutable state = Bitmask.Empty

    member this.KeyDown(time: Time, key: int) : ReplayBuilder =
        state <- state.Add(key)
        liveplay.AddFrame(time, state)
        this

    member this.KeyDown(time: Time) : ReplayBuilder = this.KeyDown(time, 0)

    member this.KeyUp(time: Time, k: int) : ReplayBuilder =
        state <- state.Remove(k)
        liveplay.AddFrame(time, state)
        this

    member this.KeyUp(time: Time) : ReplayBuilder = this.KeyUp(time, 0)

    member this.KeyDownUntil(time: Time, until: Time, key: int) : ReplayBuilder =
        this.KeyDown(time, key).KeyUp(until, key)

    member this.KeyDownUntil(time: Time, until: Time) : ReplayBuilder = this.KeyDownUntil(time, until, 0)

    member this.KeyDownFor(time: Time, duration: Time, key: int) : ReplayBuilder =
        this.KeyDownUntil(time, time + duration, key)

    member this.KeyDownFor(time: Time, duration: Time) : ReplayBuilder = this.KeyDownFor(time, duration, 0)

    member this.Build() : ReplaySource =
        liveplay.Finish()
        liveplay
