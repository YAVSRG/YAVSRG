namespace Prelude.Tests.Rulesets

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring

type ChartBuilder(keycount: int) =
    let items = ResizeArray<TimeItem<NoteRow>>()

    let mutable last_time = -Time.infinity

    member this.Note(time: Time, k: int) : ChartBuilder =

        if time > last_time then

            let row = Array.zeroCreate keycount
            row.[k] <- NoteType.NORMAL
            items.Add({ Time = time; Data = row })

            last_time <- time

        elif time = last_time then

            let row = items.[items.Count - 1].Data
            if row.[k] <> NoteType.NOTHING then failwith "Stacked note"
            row.[k] <- NoteType.NORMAL

        else failwith "Note timestamps went backwards"

        this

    member this.Note(time) = this.Note(time, 0)

    member this.Hold(time: Time, until: Time, k: int) : ChartBuilder =

        if time > last_time then

            let head = Array.zeroCreate keycount
            head.[k] <- NoteType.HOLDHEAD
            items.Add({ Time = time; Data = head })

        elif time = last_time then

            let row = items.[items.Count - 1].Data
            if row.[k] <> NoteType.NOTHING then failwith "Stacked note"
            row.[k] <- NoteType.HOLDHEAD

        else failwith "Note timestamps went backwards"

        let tail = Array.zeroCreate keycount
        tail.[k] <- NoteType.HOLDTAIL
        items.Add({ Time = until; Data = tail })

        last_time <- until

        this
    member this.Hold(time, until) = this.Hold(time, until, 0)

    member this.Build() : TimeArray<NoteRow> = items.ToArray()

type ReplayBuilder() =
    let liveplay = LiveReplay(0.0f<ms>)

    let mutable state = 0us

    member this.KeyDown(time: Time, k: int) : ReplayBuilder =
        state <- state |> Bitmask.set_key k

        liveplay.AddFrame(time, state)
        this
    member this.KeyDown(time) = this.KeyDown(time, 0)

    member this.KeyUp(time: Time, k: int) : ReplayBuilder =
        state <- state |> Bitmask.unset_key k
        liveplay.AddFrame(time, state)

        this
    member this.KeyUp(time) = this.KeyUp(time, 0)

    member this.KeyDownUntil(time: Time, until: Time, k: int) = this.KeyDown(time, k).KeyUp(until, k)
    member this.KeyDownUntil(time, until) = this.KeyDownUntil(time, until, 0)

    member this.KeyDownFor(time: Time, duration: Time, k: int) = this.KeyDownUntil(time, time + duration, k)
    member this.KeyDownFor(time, duration) = this.KeyDownFor(time, duration, 0)

    member this.Build() : IReplay =
        liveplay.Finish()
        liveplay

type GameplayEventCollector(ruleset, keys, replay, notes, rate) =
    inherit GameplayEventProcessor(ruleset, keys, replay, notes, rate)

    let events = ResizeArray<GameplayEventInternal>()

    override this.HandleEvent(event) =
        printfn "%A" event
        events.Add event

    member this.Events = events.AsReadOnly()

type ScoringEventCollector(ruleset, keys, replay, notes, rate) as this =
    inherit ScoreProcessor(ruleset, keys, replay, notes, rate)

    let events = ResizeArray<GameplayEvent>()

    do
        this.OnEvent.Add(fun event ->
            printfn "%A" event
            events.Add event
        )

    member this.Events = events.AsReadOnly()