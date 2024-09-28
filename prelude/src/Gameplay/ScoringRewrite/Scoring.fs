namespace Prelude.Gameplay.ScoringV2

open Prelude
open Prelude.Charts
open Prelude.Gameplay
open Prelude.Gameplay.RulesetsV2

type GameplayAction =
    | Hit of
        {|
            Delta: GameplayTime
            Judgement: (int * float) option
            Missed: bool
        |}
    | Hold of
        {|
            Delta: GameplayTime
            Judgement: (int * float) option
            Missed: bool
        |}
    | Release of
        {|
            Delta: GameplayTime
            Judgement: (int * float) option
            Missed: bool
            Overhold: bool
            Dropped: bool
        |}
    | DropHold
    | RegrabHold
    | GhostTap

type ScoreProcessor(ruleset: RulesetV2, keys: int, replay: IReplayProvider, notes: TimeArray<NoteRow>, rate: Rate) =
    inherit GameplayEventProcessor(ruleset, keys, replay, notes, rate)

    let hit_events = ResizeArray<GameplayEvent<GameplayAction>>()

    let on_hit_ev = Event<GameplayEvent<GameplayAction>>()
    let on_hit = on_hit_ev.Publish

    // todo: state and all the goop inside

    member private this.ProcessHit(delta, is_missed) : GameplayAction =
        Hit {| Delta = delta; Judgement = None; Missed = is_missed |}

    member private this.ProcessHold(delta, is_missed) : GameplayAction =
        Hold {| Delta = delta; Judgement = None; Missed = is_missed |}

    member private this.ProcessRelease(release_delta, missed, overhold, dropped, head_delta, missed_head) : GameplayAction =
        Release {| 
            Delta = release_delta
            Judgement = None
            Missed = missed
            Overhold = overhold
            Dropped = dropped
        |}

    member private this.ProcessDropHold() : GameplayAction =
        DropHold

    member private this.ProcessRegrabHold() : GameplayAction =
        RegrabHold

    member private this.ProcessGhostTap() : GameplayAction =
        GhostTap

    override this.HandleEvent (internal_event: GameplayEvent<GameplayActionInternal>) =
        let exposed_event : GameplayEvent<GameplayAction> =
            {
                Index = internal_event.Index
                Time = internal_event.Time
                Column = internal_event.Column
                Action =
                    match internal_event.Action with
                    | HIT(delta, is_missed) ->
                        this.ProcessHit(delta, is_missed)
                    | HOLD(delta, is_missed) ->
                        this.ProcessHold(delta, is_missed)
                    | RELEASE(release_delta, missed, overhold, dropped, head_delta, missed_head) ->
                        this.ProcessRelease(release_delta, missed, overhold, dropped, head_delta, missed_head)
                    | DROP_HOLD -> this.ProcessDropHold()
                    | REGRAB_HOLD -> this.ProcessRegrabHold()
                    | GHOST_TAP -> this.ProcessGhostTap()
            }
        hit_events.Add exposed_event
        on_hit_ev.Trigger exposed_event

    member this.Events = hit_events.AsReadOnly()
    member this.OnEvent = on_hit

module ScoreProcessor =

    let create (ruleset: RulesetV2) (keys: int) (replay: IReplayProvider) (notes: TimeArray<NoteRow>) (rate: Rate) : ScoreProcessor =
        ScoreProcessor(ruleset, keys, replay, notes, rate)

    let run (ruleset: RulesetV2) (keys: int) (replay: IReplayProvider) (notes: TimeArray<NoteRow>) (rate: Rate) : ScoreProcessor =
        let scoring = ScoreProcessor(ruleset, keys, replay, notes, rate)
        scoring.Update Time.infinity
        scoring

    open Prelude.Charts.Processing

    let create_dummy (chart: ModdedChart) : ScoreProcessor =
        let ruleset = SC.create 4
        create ruleset chart.Keys (StoredReplayProvider Array.empty) chart.Notes 1.0f<rate>
