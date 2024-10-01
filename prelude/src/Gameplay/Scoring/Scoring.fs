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

    let judgement_counts = Array.zeroCreate ruleset.Judgements.Length
    let mutable points_scored = 0.0
    let mutable max_possible_points = 0.0

    let mutable current_combo = 0
    let mutable best_combo = 0
    let mutable combo_breaks = 0
    let mutable max_possible_combo = 0

    let break_combo(would_have_increased_combo: bool) =
        if would_have_increased_combo then max_possible_combo <- max_possible_combo + 1
        combo_breaks <- combo_breaks + 1
        current_combo <- 0

    let incr_combo () =
        max_possible_combo <- max_possible_combo + 1
        current_combo <- current_combo + 1
        best_combo <- max best_combo current_combo

    let score_points(judgement_points, judgement) =
        max_possible_points <- max_possible_points + 1.0
        points_scored <- points_scored + judgement_points
        judgement_counts.[judgement] <- judgement_counts.[judgement] + 1

    let add_judgement(judgement) =
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

    let judgement_to_points : GameplayTime -> int -> float =
        match ruleset.Accuracy with
        | AccuracyPoints.WifeCurve j -> 
            fun (delta: GameplayTime) _ -> Wife3Curve.calculate j delta
        | AccuracyPoints.PointsPerJudgement weights ->
            fun _ (judgement: int) -> weights.[judgement]

    member val Duration = (TimeArray.last notes).Value.Time - (TimeArray.first notes).Value.Time
    member val JudgementCounts = judgement_counts
    member this.Accuracy = if max_possible_points = 0.0 then 1.0 else points_scored / max_possible_points
    member this.CurrentCombo = current_combo
    member this.BestCombo = best_combo
    member this.ComboBreaks = combo_breaks
    member this.MaxPossibleCombo = max_possible_combo

    member private this.ProcessHit(delta, is_missed) : GameplayAction =
            
        match ruleset.HoldMechanics with 
        | HoldMechanics.OnlyJudgeReleases _ ->
            if is_missed then break_combo(true) else incr_combo()
            Hit
                {|
                    Delta = delta
                    Judgement = None
                    Missed = is_missed
                |}
        | _ ->
            let judgement = if is_missed then ruleset.DefaultJudgement else ms_to_judgement delta
            let points = judgement_to_points delta judgement

            score_points(points, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                break_combo(true)
            else
                incr_combo()

            Hit
                {|
                    Delta = delta
                    Judgement = Some (judgement, points)
                    Missed = is_missed
                |}

    member private this.ProcessHold(delta, is_missed) : GameplayAction =

        match ruleset.HoldMechanics with
        | HoldMechanics.OnlyRequireHold _
        | HoldMechanics.JudgeReleasesSeparately _ ->
            let judgement = if is_missed then ruleset.DefaultJudgement else ms_to_judgement delta
            let points = judgement_to_points delta judgement

            score_points(points, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                break_combo(true)
            else
                incr_combo()

            Hold
                {|
                    Delta = delta
                    Judgement = Some (judgement, points)
                    Missed = is_missed
                |}

        | HoldMechanics.CombineHeadAndTail _
        | HoldMechanics.OnlyJudgeReleases _ ->
            if is_missed then break_combo(true) else incr_combo()
            Hold
                {|
                    Delta = delta
                    Judgement = None
                    Missed = is_missed
                |}

    member private this.ProcessRelease(release_delta, missed, overheld, dropped, head_delta, missed_head) : GameplayAction =
        match ruleset.HoldMechanics with
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.OsuMania windows) ->
            let judgement =
                if missed && missed_head then
                    ruleset.DefaultJudgement
                else 
                    OsuHolds.ln_judgement windows head_delta release_delta overheld dropped

            let points = judgement_to_points head_delta judgement

            score_points(points, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                break_combo(true)
            else
                incr_combo()

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
                if overheld && not dropped then
                    max head_judgement judgement_if_overheld
                elif missed || dropped then
                    max head_judgement judgement_if_dropped
                else
                    head_judgement

            let points = judgement_to_points head_delta judgement

            score_points(points, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                break_combo(true)
            else
                incr_combo()

            Release
                {|
                    Delta = release_delta
                    Judgement = Some (judgement, points)
                    Missed = missed
                    Overhold = overheld
                    Dropped = dropped
                |}

        | HoldMechanics.OnlyRequireHold _ ->
            if (not overheld) && (missed || dropped) then
                break_combo(true)
            else
                incr_combo()

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
                    
            let points = judgement_to_points release_delta judgement
            score_points(points, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                break_combo(true)
            else
                incr_combo()

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

            let points = judgement_to_points release_delta judgement

            score_points(points, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                break_combo(true)
            else
                incr_combo()

            Release
                {|
                    Delta = release_delta
                    Judgement = Some (judgement, points)
                    Missed = missed
                    Overhold = overheld
                    Dropped = dropped
                |}

    member private this.ProcessDropHold() : GameplayAction =
        break_combo (false)
        DropHold

    member private this.ProcessRegrabHold() : GameplayAction =
        RegrabHold

    member private this.ProcessGhostTap() : GameplayAction =
        // todo: rulesets can specify ghost tap judgements
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
