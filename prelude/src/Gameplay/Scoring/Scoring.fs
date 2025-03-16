namespace Prelude.Gameplay.Scoring

open System.Collections.ObjectModel
open Prelude
open Prelude.Mods
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets

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
    | GhostTap of {| Judgement: (int * float) option |}
    member this.Judgement: (int * float) option =
        match this with
        | Hit h -> h.Judgement
        | Hold h -> h.Judgement
        | Release r -> r.Judgement
        | DropHold
        | RegrabHold -> None
        | GhostTap g -> g.Judgement

[<Struct>]
type ComboAction =
    | Increase
    | Break of instead_of_increase: bool
    | NoChange

type GameplayEvent =
    {
        Index: int
        Time: ChartTime
        Column: int
        Action: GameplayAction
        Combo: ComboAction
    }

/// This processor extends GameplayEventProcessor, taking its series of raw `GameplayEvent<GameplayActionInternal>` event markers
/// These raw events are converted to judgements, points, combo changes etc according to the ruleset and then output as `GameplayEvent<GameplayAction>` event markers
/// `GameplayEvent<GameplayAction>` event markers are subscribable and exposed as a list, they power everything the gameplay HUD, score screen and playfield display in the client

type ScoreProcessor(ruleset: Ruleset, keys: int, replay: IReplayProvider, notes: TimeArray<NoteRow>, rate: Rate) =
    inherit GameplayEventProcessor(ruleset, keys, replay, notes, rate)

    let hit_events = ResizeArray<GameplayEvent>()

    let on_event_ev = Event<GameplayEvent>()
    let on_event = on_event_ev.Publish

    let judgement_counts = Array.zeroCreate ruleset.Judgements.Length
    let mutable points_scored = 0.0
    let mutable max_possible_points = 0.0

    let mutable current_combo = 0
    let mutable best_combo = 0
    let mutable combo_breaks = 0
    let mutable max_possible_combo = 0

    let break_combo (would_have_increased_combo: bool) =
        if would_have_increased_combo then max_possible_combo <- max_possible_combo + 1
        combo_breaks <- combo_breaks + 1
        current_combo <- 0

    let incr_combo () =
        max_possible_combo <- max_possible_combo + 1
        current_combo <- current_combo + 1
        best_combo <- max best_combo current_combo

    let score_points (judgement_points: float, judgement: int) =
        max_possible_points <- max_possible_points + 1.0
        points_scored <- points_scored + judgement_points
        judgement_counts.[judgement] <- judgement_counts.[judgement] + 1

    let add_judgement (judgement: int) =
        judgement_counts.[judgement] <- judgement_counts.[judgement] + 1

    let ms_to_judgement (delta: GameplayTime) : int =
        let mutable j = 0
        while
            j + 1 < ruleset.Judgements.Length
            && (
                match ruleset.Judgements.[j].TimingWindows with
                | None -> true
                | Some (early, late) -> delta < early || delta > late
            )
            do
            j <- j + 1
        j

    let judgement_or_ms_to_points : GameplayTime -> int -> float =
        match ruleset.Accuracy with
        | AccuracyPoints.WifeCurve j ->
            fun (delta: GameplayTime) _ -> Wife3Curve.calculate j delta
        | AccuracyPoints.PointsPerJudgement weights ->
            fun _ (judgement: int) -> weights.[judgement]

    let judgement_to_points (judgement: int) : float =
        match ruleset.Accuracy with
        | AccuracyPoints.PointsPerJudgement weights -> weights.[judgement]
        | _ -> failwith "Ruleset isn't using PointsPerJudgement + a feature that expects it to be used. This line should be unreachable if the ruleset was validated"

    member val Duration : Time = (TimeArray.last notes).Value.Time - (TimeArray.first notes).Value.Time
    member val JudgementCounts : int array = judgement_counts
    member val Rate : Rate = rate
    member val Keys : int = keys
    member val internal Notes : TimeArray<NoteRow> = notes

    member this.Accuracy : float = if max_possible_points = 0.0 then 1.0 else points_scored / max_possible_points
    member this.FormattedAccuracy : string = ruleset.FormatAccuracy this.Accuracy

    member this.CurrentCombo : int = current_combo
    member this.BestCombo : int = best_combo
    member this.ComboBreaks : int = combo_breaks
    member this.MaxPossibleCombo : int = max_possible_combo

    member this.MaxPossiblePoints : float = max_possible_points
    member this.PointsScored : float = points_scored

    member this.Events : ReadOnlyCollection<GameplayEvent> = hit_events.AsReadOnly()
    member this.OnEvent : IEvent<GameplayEvent> = on_event

    /// Throws an exception if used on a live/online replay
    member this.Recreate(ruleset: Ruleset) = ScoreProcessor(ruleset, keys, replay.GetFullReplay() |> StoredReplayProvider, notes, rate)
    member this.Recreate() = this.Recreate(ruleset)

    member private this.ProcessHit(delta: GameplayTime, is_missed: bool) : ComboAction * GameplayAction =

        match ruleset.HoldMechanics with
        | HoldMechanics.OnlyJudgeReleases _ ->
            (if is_missed then Break true else Increase),
            Hit
                {|
                    Delta = delta
                    Judgement = None
                    Missed = is_missed
                |}
        | _ ->
            let judgement = if is_missed then ruleset.DefaultJudgement else ms_to_judgement delta
            let points = judgement_or_ms_to_points delta judgement

            score_points(points, judgement)

            (if ruleset.Judgements.[judgement].BreaksCombo then Break true else Increase),
            Hit
                {|
                    Delta = delta
                    Judgement = Some (judgement, points)
                    Missed = is_missed
                |}

    member private this.ProcessHold(delta: GameplayTime, is_missed: bool) : ComboAction * GameplayAction =

        match ruleset.HoldMechanics with
        | HoldMechanics.OnlyRequireHold _
        | HoldMechanics.JudgeReleasesSeparately _ ->
            let judgement = if is_missed then ruleset.DefaultJudgement else ms_to_judgement delta
            let points = judgement_or_ms_to_points delta judgement

            score_points(points, judgement)

            (if ruleset.Judgements.[judgement].BreaksCombo then Break true else Increase),
            Hold
                {|
                    Delta = delta
                    Judgement = Some (judgement, points)
                    Missed = is_missed
                |}

        | HoldMechanics.CombineHeadAndTail _
        | HoldMechanics.OnlyJudgeReleases _ ->
            (if is_missed then Break true else Increase),
            Hold
                {|
                    Delta = delta
                    Judgement = None
                    Missed = is_missed
                |}

    member private this.ProcessRelease(release_delta: GameplayTime, missed: bool, overheld: bool, dropped: bool, head_delta: GameplayTime, missed_head: bool) : ComboAction * GameplayAction =
        match ruleset.HoldMechanics with
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.OsuMania windows) ->
            let judgement =
                if missed && missed_head && not overheld then
                    ruleset.DefaultJudgement
                else
                    OsuHolds.ln_judgement windows head_delta release_delta overheld dropped

            let points = judgement_to_points judgement

            score_points(points, judgement)

            (if ruleset.Judgements.[judgement].BreaksCombo then Break true else Increase),
            Release
                {|
                    Delta = release_delta
                    Judgement = Some (judgement, points)
                    Missed = missed
                    Overhold = overheld
                    Dropped = dropped
                |}

        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr (_, _, judgement_if_dropped, judgement_if_overheld)) ->
            let head_judgement = ms_to_judgement head_delta
            let judgement =
                if missed_head && missed then ruleset.DefaultJudgement
                elif overheld && not dropped then max head_judgement judgement_if_overheld
                elif dropped then max head_judgement judgement_if_dropped
                else head_judgement

            let points = judgement_to_points judgement

            score_points(points, judgement)

            (if ruleset.Judgements.[judgement].BreaksCombo then Break true else Increase),
            Release
                {|
                    Delta = release_delta
                    Judgement = Some (judgement, points)
                    Missed = missed
                    Overhold = overheld
                    Dropped = dropped
                |}

        | HoldMechanics.OnlyRequireHold _ ->

            (if (not overheld) && (missed || dropped) then Break true else Increase),
            Release
                {|
                    Delta = release_delta
                    Judgement = None
                    Missed = missed
                    Overhold = overheld
                    Dropped = dropped
                |}

        | HoldMechanics.JudgeReleasesSeparately (windows, judgement_if_overheld) ->
            let judgement =
                if overheld then judgement_if_overheld
                elif missed then ruleset.DefaultJudgement
                else
                    assert(windows.Length = ruleset.Judgements.Length)
                    let mutable j = 0
                    while
                        j + 1 < ruleset.Judgements.Length
                        && (
                            match windows.[j] with
                            | None -> true
                            | Some (early, late) -> release_delta < early || release_delta > late
                        )
                        do
                        j <- j + 1
                    j

            let points = judgement_or_ms_to_points release_delta judgement
            score_points(points, judgement)

            (if ruleset.Judgements.[judgement].BreaksCombo then Break true else Increase),
            Release
                {|
                    Delta = release_delta
                    Judgement = Some (judgement, points)
                    Missed = missed
                    Overhold = overheld
                    Dropped = dropped
                |}

        | HoldMechanics.OnlyJudgeReleases judgement_if_dropped ->
            let judgement =
                if missed then
                    ruleset.DefaultJudgement
                else
                    ms_to_judgement release_delta
                    |> if dropped || overheld then max judgement_if_dropped else id

            let points = judgement_or_ms_to_points release_delta judgement

            score_points(points, judgement)

            (if ruleset.Judgements.[judgement].BreaksCombo then Break true else Increase),
            Release
                {|
                    Delta = release_delta
                    Judgement = Some (judgement, points)
                    Missed = missed
                    Overhold = overheld
                    Dropped = dropped
                |}

    member private this.ProcessDropHold() : ComboAction * GameplayAction =
        Break false,
        DropHold

    member private this.ProcessRegrabHold() : ComboAction * GameplayAction =
        NoChange,
        RegrabHold

    member private this.ProcessGhostTap() : ComboAction * GameplayAction =
        match ruleset.HitMechanics.GhostTapJudgement with
        | Some judgement ->
            let points = judgement_to_points judgement

            score_points(points, judgement)

            (if ruleset.Judgements.[judgement].BreaksCombo then Break true else NoChange),
            GhostTap {| Judgement = Some (judgement, points) |}
        | None ->
        NoChange,
        GhostTap {| Judgement = None |}

    override this.HandleEvent (internal_event: GameplayEventInternal) =
        let combo_action, gameplay_action =
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

        match combo_action with
        | NoChange -> ()
        | Increase -> incr_combo ()
        | Break instead_of_increase -> break_combo instead_of_increase

        let gameplay_event : GameplayEvent =
            {
                Index = internal_event.Index
                Time = internal_event.Time
                Column = internal_event.Column
                Action = gameplay_action
                Combo = combo_action
            }

        hit_events.Add gameplay_event
        on_event_ev.Trigger gameplay_event

module ScoreProcessor =

    let create (ruleset: Ruleset) (keys: int) (replay: IReplayProvider) (notes: TimeArray<NoteRow>) (rate: Rate) : ScoreProcessor =
        ScoreProcessor(ruleset, keys, replay, notes, rate)

    let run (ruleset: Ruleset) (keys: int) (replay: IReplayProvider) (notes: TimeArray<NoteRow>) (rate: Rate) : ScoreProcessor =
        let scoring = ScoreProcessor(ruleset, keys, replay, notes, rate)
        scoring.Update Time.infinity
        scoring

    let create_dummy (chart: ModdedChart) : ScoreProcessor =
        create SC_J4 chart.Keys (StoredReplayProvider Array.empty) chart.Notes 1.0f<rate>