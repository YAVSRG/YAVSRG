namespace Prelude.Scoring

open System
open Prelude.Common
open Prelude.ChartFormats.Interlude

type HitEventGutsInternal =
    | Hit_ of
        delta: Time *
        isHoldHead: bool *
        missed: bool
    | Release_ of
        delta: Time *
        missed: bool *
        overhold: bool *
        dropped: bool

type HitEventGuts =
    | Hit of 
        {| 
            Judgement: JudgementId option
            Missed: bool
            Delta: Time
            /// True if this is the head of a hold note
            IsHold: bool
        |}
    | Release of
        {| 
            Judgement: JudgementId option
            Missed: bool
            Delta: Time
            /// True if the hold was pressed correctly and held too long, past the window to release it
            Overhold: bool 
            /// True if at any point (including missing the head) the hold was released when it shouldn't have been
            Dropped: bool
        |}

type HitEvent<'Guts> =
    {
        Time: ChartTime
        Column: int
        Guts: 'Guts
    }

(*
    Health bar system to be attached to other metrics (Bars are dependent on/coupled with the judgements scored according to other systems)
    These maintain a meter that fills as you hit notes well, and depletes as you hit notes badly (or miss)
    Falling below a certain level of "health" causes you to "fail" a chart
        Players can choose what the consequences of for failure (ending play immediately, just a flag on score screen, etc)

    In some games health bar is the main metric for competence at a chart
        Here its purpose is primarily to push players away from charts that are clearly too hard rather than being the arbiter
    (But things are complete enough here for you to simulate clear-oriented play as in those certain games e.g. BMS)
*)

type HealthBarState =
    {
        mutable HasFailed: bool
        mutable CurrentlyFailed: bool
        mutable Health: float
    }

type HealthBarMetric(config: HealthBarConfig) =

    member val State: HealthBarState = 
        { 
            HasFailed = false
            CurrentlyFailed = false
            Health = config.StartingHealth
        }

    member this.ChangeHP (x: float) =
        let newHP = Math.Clamp(this.State.Health + x, 0.0, 1.0)
        this.State.Health <- newHP
        if this.FailCondition newHP then
            this.State.HasFailed <- true
            this.State.CurrentlyFailed <- true
        else this.State.CurrentlyFailed <- false
        
    member this.Failed = if config.OnlyFailAtEnd then this.State.CurrentlyFailed else this.State.HasFailed
    member this.Format() = sprintf "%.2f%%" (this.State.Health * 100.0)

    member this.FailCondition hp = hp <= config.ClearThreshold

    member this.HandleEvent ev =
        match ev.Guts with
        | Hit evData -> match evData.Judgement with Some j -> this.ChangeHP(config.Points[j]) | None -> ()
        | Release evData -> match evData.Judgement with Some j -> this.ChangeHP(config.Points[j]) | None -> ()

(*
    Accuracy/scoring system metric.
    Each note you hit is assigned a certain number of points - Your % accuracy is points scored out of the possible maximum.
    Combo/combo breaking also built-in - Your combo is the number of notes hit well in a row
*)

type AccuracySystemState =
    {
        Judgements: int array
        mutable PointsScored: float
        mutable MaxPointsScored: float
        mutable CurrentCombo: int
        mutable BestCombo: int
        mutable MaxPossibleCombo: int
        mutable ComboBreaks: int
    }
    member this.BreakCombo (wouldHaveIncreasedCombo: bool) =
        if wouldHaveIncreasedCombo then this.MaxPossibleCombo <- this.MaxPossibleCombo + 1
        this.CurrentCombo <- 0
        this.ComboBreaks <- this.ComboBreaks + 1

    member this.IncrCombo() =
        this.MaxPossibleCombo <- this.MaxPossibleCombo + 1
        this.CurrentCombo <- this.CurrentCombo + 1
        this.BestCombo <- Math.Max(this.CurrentCombo, this.BestCombo)

    member this.Add(points: float, maxpoints: float, judge: JudgementId) =
        this.PointsScored <- this.PointsScored + points
        this.MaxPointsScored <- this.MaxPointsScored + maxpoints
        this.Judgements.[judge] <- this.Judgements.[judge] + 1

    member this.Add(judge: JudgementId) = this.Add(0.0, 0.0, judge)

type private HoldState =
    | Nothing
    | Holding
    | Dropped
    | MissedHead

[<AbstractClass>]
type IScoreMetric
    (
        config: Ruleset,
        healthBar: HealthBarMetric,
        keys: int,
        replayProvider: IReplayProvider,
        notes: TimeData<NoteRow>,
        rate: float32
    ) =
    inherit ReplayConsumer(keys, replayProvider)

    let firstNote = offsetOf notes.First.Value
    let missWindow = config.Accuracy.MissWindow * rate

    // having two seekers improves performance when feeding scores rather than playing live
    let mutable noteSeekPassive = 0
    let mutable noteSeekActive = 0

    let internalHoldStates = Array.create keys (Nothing, -1)

    let hitData = InternalScore.createDefault config.Accuracy.MissWindow keys notes
    let hitEvents = ResizeArray<HitEvent<HitEventGuts>>()

    let mutable hitCallback = fun ev -> ()

    member val State =
        {
            Judgements = Array.zeroCreate config.Judgements.Length
            PointsScored = 0.0
            MaxPointsScored = 0.0
            CurrentCombo = 0
            BestCombo = 0
            ComboBreaks = 0
            MaxPossibleCombo = 0
        }

    member this.Name = config.Name
    member this.Value =
        let v = this.State.PointsScored / this.State.MaxPointsScored
        if Double.IsNaN v then 1.0 else v
    member this.FormatAccuracy() = sprintf "%.2f%%" (this.Value * 100.0)
    member this.HP = healthBar
    member this.MissWindow = config.Accuracy.MissWindow
    member this.ScaledMissWindow = missWindow

    member this.IsHoldDropped (index: int) (k: int) =
        match internalHoldStates.[k] with
        | Dropped, i | MissedHead, i when i + 1 >= index -> true
        | _ -> false

    member this.HitData = hitData

    member this.SetHitCallback (func: HitEvent<HitEventGuts> -> unit) = hitCallback <- func

    member this.Finished = noteSeekPassive = hitData.Length

    member this.HitEvents = hitEvents.AsReadOnly()

    // correctness guaranteed up to the time you update, no matter how you update
    // call Update with Time.infinity to do a correct feeding of the whole replay
    member this.Update (relativeTime: ChartTime) =
        this.PollReplay relativeTime // calls HandleKeyDown and HandleKeyUp appropriately
        this.HandlePassive relativeTime

    member private this.HandlePassive (relativeTime: ChartTime) =
        let now = firstNote + relativeTime
        let target = now - missWindow
        while noteSeekPassive < hitData.Length && InternalScore.offsetOf hitData.[noteSeekPassive] <= target do
            let struct (t, deltas, status) = hitData.[noteSeekPassive]
            for k = 0 to (keys - 1) do

                if status.[k] = HitStatus.HIT_REQUIRED then
                    this._HandleEvent { Time = t - firstNote + missWindow; Column = k; Guts = Hit_ (deltas.[k], false, true) }

                elif status.[k] = HitStatus.HIT_HOLD_REQUIRED then
                    internalHoldStates.[k] <- MissedHead, noteSeekPassive
                    this._HandleEvent { Time = t - firstNote + missWindow; Column = k; Guts = Hit_ (deltas.[k], true, true) }

                elif status.[k] = HitStatus.RELEASE_REQUIRED then
                    let overhold =
                        match internalHoldStates.[k] with
                        | Dropped, i | Holding, i when i <= noteSeekPassive -> true
                        | _ -> false
                        && Bitmap.hasBit k this.KeyState
                    let dropped =
                        match internalHoldStates.[k] with
                        | Dropped, _
                        | MissedHead, _ -> true
                        | _ -> false
                    this._HandleEvent { Time = t - firstNote + missWindow; Column = k; Guts = Release_ (deltas.[k], true, overhold, dropped) }
                    match internalHoldStates.[k] with
                    | _, i when i < noteSeekPassive -> internalHoldStates.[k] <- Nothing, noteSeekPassive
                    | _ -> ()

            noteSeekPassive <- noteSeekPassive + 1

    override this.HandleKeyDown (relativeTime: ChartTime, k: int) =
        this.HandlePassive relativeTime
        let now = firstNote + relativeTime
        while noteSeekActive < hitData.Length && InternalScore.offsetOf hitData.[noteSeekActive] < now - missWindow do 
            noteSeekActive <- noteSeekActive + 1

        let mutable i = noteSeekActive
        let mutable delta = missWindow
        let mutable found = -1
        let target = now + missWindow

        while i < hitData.Length && InternalScore.offsetOf hitData.[i] <= target do
            let struct (t, deltas, status) = hitData.[i]
            let d = now - t
            if status.[k] = HitStatus.HIT_REQUIRED || status.[k] = HitStatus.HIT_HOLD_REQUIRED then
                if (Time.Abs delta > Time.Abs d) then
                    found <- i
                    delta <- d
            // Accept a hit that looks like it's intended for a previous badly hit note that was fumbled early (preventing column lock)
            elif status.[k] = HitStatus.HIT_ACCEPTED && deltas.[k] < -config.Accuracy.CbrushWindow then
                if (Time.Abs delta > Time.Abs d) then
                    found <- i
                    delta <- d
            i <- i + 1

        if found >= 0 then
            let struct (t, deltas, status) = hitData.[found]
            if status.[k] <> HitStatus.HIT_ACCEPTED then // Could be an already hit note, in which case just swallow the extra input
                let isHoldHead = status.[k] <> HitStatus.HIT_REQUIRED
                status.[k] <- HitStatus.HIT_ACCEPTED
                deltas.[k] <- delta / rate
                this._HandleEvent { Time = relativeTime; Column = k; Guts = Hit_ (deltas.[k], isHoldHead, false) }
                // Begin tracking if it's a hold note
                //assert(fst internalHoldStates.[k] = Nothing)
                if isHoldHead then internalHoldStates.[k] <- Holding, found
        else // If no note to hit, but a hold note head was missed, pressing key marks it dropped instead
            internalHoldStates.[k] <- 
                match internalHoldStates.[k] with
                | MissedHead, i -> Dropped, i
                | x -> x
                
    override this.HandleKeyUp (relativeTime: ChartTime, k: int) =
        this.HandlePassive relativeTime
        let now = firstNote + relativeTime
        match internalHoldStates.[k] with
        | Holding, holdHeadIndex
        | Dropped, holdHeadIndex ->

            let mutable i = holdHeadIndex + 1
            let mutable delta = missWindow
            let mutable found = -1
            let target = now + missWindow

            while i < hitData.Length && InternalScore.offsetOf hitData.[i] <= target do
                let struct (t, _, status) = hitData.[i]
                let d = now - t
                if status.[k] = HitStatus.RELEASE_REQUIRED then
                    // Get the first unreleased hold tail we see, after the head of the hold we're tracking
                    found <- i
                    delta <- d
                    i <- hitData.Length
                i <- i + 1

            if found >= 0 then
                let struct (t, deltas, status) = hitData.[found]
                status.[k] <- HitStatus.RELEASE_ACCEPTED
                deltas.[k] <- delta / rate
                this._HandleEvent { Time = relativeTime; Column = k; Guts = Release_ (deltas.[k], false, false, fst internalHoldStates.[k] = Dropped) }
                internalHoldStates.[k] <- Nothing, found
            else // If we released but too early (no sign of the tail within range) make the long note dropped
                internalHoldStates.[k] <- 
                    match internalHoldStates.[k] with
                    | Holding, i -> Dropped, i
                    | x -> x
        | MissedHead, _
        | Nothing, _ -> ()
    
    abstract member HandleEvent : HitEvent<HitEventGutsInternal> -> HitEvent<HitEventGuts>
    member private this._HandleEvent ev =
        let ev = this.HandleEvent ev
        hitEvents.Add ev
        hitCallback ev
        healthBar.HandleEvent ev

module Helpers =

    let window_func (default_judge: JudgementId) (gates: (Time * JudgementId) list) (delta: Time) : JudgementId =
        let rec loop gates =
            match gates with
            | [] -> default_judge
            | (w, j) :: xs ->
                if delta < w then j else loop xs
        loop gates

    let points (conf: Ruleset) (delta: Time) (judge: JudgementId) : float =
        match conf.Accuracy.Points with
        | AccuracyPoints.WifeCurve j -> Rulesets.Wife.wife_curve j delta
        | AccuracyPoints.Weights (maxweight, weights) -> weights.[judge] / maxweight

// Concrete implementation of rulesets

type CustomScoring(config: Ruleset, keys, replay, notes, rate) =
    inherit IScoreMetric(config, HealthBarMetric(config.Health), keys, replay, notes, rate)

    let headJudgements = Array.create keys config.DefaultJudgement
    let headDeltas = Array.create keys config.Accuracy.MissWindow

    let point_func = Helpers.points config
    let window_func = Helpers.window_func config.DefaultJudgement config.Accuracy.Timegates

    override this.HandleEvent ev =
        { 
            Time = ev.Time
            Column = ev.Column
            Guts = 
                match ev.Guts with
                | Hit_ (delta, isHold, missed) ->
                    let judgement = window_func delta
                    if isHold then
                        headJudgements.[ev.Column] <- judgement
                        headDeltas.[ev.Column] <- delta

                        match config.Accuracy.HoldNoteBehaviour with
                        | HoldNoteBehaviour.JustBreakCombo
                        | HoldNoteBehaviour.JudgeReleases _ -> 
                            this.State.Add(point_func delta judgement, 1.0, judgement)
                            if config.Judgements.[judgement].BreaksCombo then this.State.BreakCombo true else this.State.IncrCombo()
                            Hit {| Judgement = Some judgement; Missed = missed; Delta = delta; IsHold = isHold |}

                        | HoldNoteBehaviour.Osu _
                        | HoldNoteBehaviour.Normal _
                        | HoldNoteBehaviour.OnlyJudgeReleases ->
                            Hit {| Judgement = None; Missed = missed; Delta = delta; IsHold = isHold |}
                    else
                        this.State.Add(point_func delta judgement, 1.0, judgement)
                        if config.Judgements.[judgement].BreaksCombo then this.State.BreakCombo true else this.State.IncrCombo()
                        Hit {| Judgement = Some judgement; Missed = missed; Delta = delta; IsHold = isHold |}

                | Release_ (delta, missed, overhold, dropped) ->
                    let headJudgement = headJudgements.[ev.Column]

                    match config.Accuracy.HoldNoteBehaviour with
                    | HoldNoteBehaviour.Osu od ->
                        let judgement = Rulesets.Osu.ln_judgement od headDeltas.[ev.Column] delta overhold dropped
                        this.State.Add(point_func delta judgement, 1.0, judgement)
                        if config.Judgements.[judgement].BreaksCombo then this.State.BreakCombo true else this.State.IncrCombo()
                        Release {| Judgement = Some judgement; Missed = missed; Delta = delta; Overhold = overhold; Dropped = dropped |}

                    | HoldNoteBehaviour.JustBreakCombo ->
                        if missed || dropped then this.State.BreakCombo true else this.State.IncrCombo()
                        Release {| Judgement = None; Missed = missed; Delta = delta; Overhold = overhold; Dropped = dropped |}

                    | HoldNoteBehaviour.JudgeReleases d -> 
                        let judgement = Helpers.window_func config.DefaultJudgement d.Timegates delta
                        this.State.Add(point_func delta judgement, 1.0, judgement)
                        if config.Judgements.[judgement].BreaksCombo then this.State.BreakCombo true else this.State.IncrCombo()
                        Release {| Judgement = Some judgement; Missed = missed; Delta = delta; Overhold = overhold; Dropped = dropped |}

                    | HoldNoteBehaviour.Normal rules ->
                        let judgement =
                            if missed || dropped then max headJudgement rules.JudgementIfDropped
                            elif overhold then max headJudgement rules.JudgementIfOverheld
                            else headJudgement
                        this.State.Add(point_func delta judgement, 1.0, judgement)
                        if config.Judgements.[judgement].BreaksCombo then this.State.BreakCombo true else this.State.IncrCombo()
                        Release {| Judgement = Some judgement; Missed = missed; Delta = delta; Overhold = overhold; Dropped = dropped |}

                    | HoldNoteBehaviour.OnlyJudgeReleases ->
                        let judgement = window_func delta
                        this.State.Add(point_func delta judgement, 1.0, judgement)
                        if config.Judgements.[judgement].BreaksCombo then this.State.BreakCombo true else this.State.IncrCombo()
                        Release {| Judgement = Some judgement; Missed = missed; Delta = delta; Overhold = overhold; Dropped = dropped |}
        }

module Metrics =
    
    let createScoreMetric config keys (replay: IReplayProvider) notes rate : IScoreMetric =
        CustomScoring(config, keys, replay, notes, rate)

    let createDummyMetric (chart: Chart) : IScoreMetric =
        createScoreMetric (Rulesets.SC.create 4) chart.Keys (StoredReplayProvider Array.empty) chart.Notes 1.0f