namespace Prelude.Tests.Scoring

open Prelude
open Prelude.Charts
open Prelude.Gameplay
open Prelude.Gameplay.ScoringV2

module Helpers =

    type ChartBuilder(keycount: int) =
        let items = ResizeArray<TimeItem<NoteRow>>()

        member this.Note(time: Time) : ChartBuilder =
            let row = Array.zeroCreate keycount
            row.[0] <- NoteType.NORMAL
            items.Add({ Time = time; Data = row })

            this

        member this.Hold(time: Time, until: Time) : ChartBuilder =
            let head = Array.zeroCreate keycount
            head.[0] <- NoteType.HOLDHEAD
            items.Add({ Time = time; Data = head })

            let tail = Array.zeroCreate keycount
            tail.[0] <- NoteType.HOLDTAIL
            items.Add({ Time = until; Data = tail })

            this

        member this.Build() : TimeArray<NoteRow> = items.ToArray()

    type ReplayBuilder() =
        let liveplay = LiveReplayProvider(0.0f<ms>)

        let mutable state = 0us

        member this.KeyDown(time: Time, k: int) : ReplayBuilder =
            state <- state |> Bitmask.set_key k

            liveplay.Add(time, state)
            this

        member this.KeyUp(time: Time, k: int) : ReplayBuilder =
            state <- state |> Bitmask.unset_key k
            liveplay.Add(time, state)

            this

        member this.KeyDownUntil(time: Time, until: Time, k: int) =
            this.KeyDown(time, k).KeyUp(until, k)

        member this.KeyDownFor(time: Time, duration: Time, k: int) =
            this.KeyDown(time, k).KeyUp(time + duration, k)

        member this.Build() : IReplayProvider = liveplay

    type GameplayEventCollector(ruleset, keys, replay, notes, rate) =
        inherit GameplayEventProcessor(ruleset, keys, replay, notes, rate)

        let events = ResizeArray<GameplayEvent<GameplayActionInternal>>()

        override this.HandleEvent(event) =
            printfn "%A" event
            events.Add event

        member this.Events = events.AsReadOnly()