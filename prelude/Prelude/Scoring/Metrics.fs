namespace Prelude.Scoring

open System
open Prelude.Common
open Prelude.ChartFormats.Interlude

// user-facing flags of "judgements", how well you hit notes
type JudgementType =
    | RIDICULOUS = 0
    | MARVELLOUS = 1
    | PERFECT = 2
    | GREAT = 3
    | GOOD = 4
    | BAD = 5
    | MISS = 6
module JudgementType = let count = 7

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
            Judgement: JudgementType option
            Missed: bool
            Delta: Time
            /// True if this is the head of a hold note
            IsHold: bool
        |}
    | Release of
        {| 
            Judgement: JudgementType option
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

[<AbstractClass>]
type IHealthBarSystem
    (
        name: string,
        startingHealth: float,
        onlyFailAtEnd: bool
    ) =

    let state: HealthBarState = { HasFailed = false; CurrentlyFailed = false; Health = startingHealth }

    member this.ChangeHP (x: float) =
        let newHP = Math.Clamp(state.Health + x, 0.0, 1.0)
        state.Health <- newHP
        if this.FailCondition newHP then
            state.HasFailed <- true
            state.CurrentlyFailed <- true
        else state.CurrentlyFailed <- false

    abstract member FailCondition : float -> bool
    abstract member HandleEvent : HitEvent<HitEventGuts> -> unit
        
    member this.Failed = if onlyFailAtEnd then state.CurrentlyFailed else state.HasFailed
    member this.Name = name
    member this.Format() = sprintf "%.2f%%" (state.Health * 100.0)

type HealthBarPointsSystem
    (
        name: string,
        startingHealth: float,
        onlyFailAtEnd: bool,
        points_func: JudgementType -> float,
        clear_threshold: float
    ) =
    inherit IHealthBarSystem(name, startingHealth, onlyFailAtEnd)

    override this.FailCondition hp = hp <= clear_threshold

    override this.HandleEvent ev =
        match ev.Guts with
        | Hit evData -> match evData.Judgement with Some j -> this.ChangeHP(points_func j) | None -> ()
        | Release evData -> match evData.Judgement with Some j -> this.ChangeHP(points_func j) | None -> ()

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
        mutable ComboBreaks: int
    }
    // helpers to reduce on duplicating this logic/making mistakes later
    member this.BreakCombo() =
        this.CurrentCombo <- 0
        this.ComboBreaks <- this.ComboBreaks + 1
    member this.IncrCombo() =
        this.CurrentCombo <- this.CurrentCombo + 1
        this.BestCombo <- Math.Max(this.CurrentCombo, this.BestCombo)
    member this.Add(points: float, maxpoints: float, judge: JudgementType) =
        this.PointsScored <- this.PointsScored + points
        this.MaxPointsScored <- this.MaxPointsScored + maxpoints
        this.Judgements.[int judge] <- this.Judgements.[int judge] + 1
    member this.Add(judge: JudgementType) = this.Add(0.0, 0.0, judge)

type private InternalHoldState =
    | Nothing = 0
    | Holding = 1
    | Dropped = 2
    | MissedHead = 3

[<AbstractClass>]
type IScoreMetric
    (
        name: string,
        healthBar: IHealthBarSystem,
        rawMissWindow: Time,
        cbrushWindow: Time,
        keys: int,
        replayProvider: IReplayProvider,
        notes: TimeData<NoteRow>,
        rate: float32
    ) =
    inherit ReplayConsumer(keys, replayProvider)

    let firstNote = offsetOf notes.First.Value
    let missWindow = rawMissWindow * rate

    // having two seekers improves performance when feeding scores rather than playing live
    let mutable noteSeekPassive = 0
    let mutable noteSeekActive = 0

    let internalHoldStates = Array.create keys InternalHoldState.Nothing

    let hitData = InternalScore.createDefault rawMissWindow keys notes
    let hitEvents = ResizeArray<HitEvent<HitEventGuts>>()

    let mutable hitCallback = fun ev -> ()

    new(name, hp, misswindow, keys, replay, notes, rate) = IScoreMetric(name, hp, misswindow, misswindow, keys, replay, notes, rate)

    member val State =
        {
            Judgements = Array.zeroCreate JudgementType.count
            PointsScored = 0.0
            MaxPointsScored = 0.0
            CurrentCombo = 0
            BestCombo = 0
            ComboBreaks = 0
        }

    member this.Name = name
    member this.Value =
        let v = this.State.PointsScored / this.State.MaxPointsScored
        if Double.IsNaN v then 1.0 else v
    member this.FormatAccuracy() = sprintf "%.2f%%" (this.Value * 100.0)
    member this.HP = healthBar
    member this.MissWindow = rawMissWindow
    member this.ScaledMissWindow = missWindow

    member this.HitData = hitData

    member this.SetHitCallback (func: HitEvent<HitEventGuts> -> unit) = hitCallback <- func

    member this.Finished = noteSeekPassive = hitData.Length

    member this.HitEvents = hitEvents.AsReadOnly()

    // correctness guaranteed up to the time you update, no matter how you update
    // call Update with Time.infinity to do a correct feeding of the whole replay
    member this.Update (time: ChartTime) =
        this.PollReplay time // calls HandleKeyDown and HandleKeyUp appropriately
        this.HandlePassive time

    member private this.HandlePassive (relativeTime: ChartTime) =
        let now = firstNote + relativeTime
        let target = now - missWindow
        while noteSeekPassive < hitData.Length && InternalScore.offsetOf hitData.[noteSeekPassive] <= target do
            let struct (t, deltas, status) = hitData.[noteSeekPassive]
            for k = 0 to (keys - 1) do

                if status.[k] = HitStatus.HIT_REQUIRED then
                    this._HandleEvent { Time = t - firstNote + missWindow; Column = k; Guts = Hit_ (deltas.[k], false, true) }

                elif status.[k] = HitStatus.HIT_HOLD_REQUIRED then
                    internalHoldStates.[k] <- InternalHoldState.MissedHead
                    this._HandleEvent { Time = t - firstNote + missWindow; Column = k; Guts = Hit_ (deltas.[k], true, true) }

                elif status.[k] = HitStatus.RELEASE_REQUIRED then
                    let overhold = internalHoldStates.[k] = InternalHoldState.Dropped && Bitmap.hasBit k this.KeyState
                    let dropped = internalHoldStates.[k] = InternalHoldState.Dropped || internalHoldStates.[k] = InternalHoldState.MissedHead
                    this._HandleEvent { Time = t - firstNote + missWindow; Column = k; Guts = Release_ (deltas.[k], true, overhold, dropped) }
                    internalHoldStates.[k] <- InternalHoldState.Nothing

            noteSeekPassive <- noteSeekPassive + 1
        // missing: checks key up as hold middle passes receptors
        // algorithm must "forget" what it finds if we see a hold head or tail within misswindow

    override this.HandleKeyDown (relativeTime: ChartTime, k: int) =
        this.HandlePassive relativeTime
        let now = firstNote + relativeTime
        while noteSeekActive < hitData.Length && InternalScore.offsetOf hitData.[noteSeekActive] < now - missWindow do 
            noteSeekActive <- noteSeekActive + 1
        let mutable i = noteSeekActive
        let mutable delta = missWindow
        let mutable found = -1
        let mutable priority = -1
        let target = now + missWindow
        while i < hitData.Length && InternalScore.offsetOf hitData.[i] <= target do
            let struct (t, deltas, status) = hitData.[i]
            let d = now - t
            if status.[k] = HitStatus.HIT_REQUIRED || status.[k] = HitStatus.HIT_HOLD_REQUIRED then
                if (Time.Abs delta > Time.Abs d) || priority < 1 then
                    priority <- 1
                    found <- i
                    delta <- d
            // absorb a hit that looks like it's intended for a previous badly hit note that was fumbled early (preventing column lock)
            elif status.[k] = HitStatus.HIT_ACCEPTED && deltas.[k] < -cbrushWindow then
                if (Time.Abs delta > Time.Abs d) || priority < 0 then
                    priority <- 1
                    found <- i
                    delta <- d
            i <- i + 1
        if found >= 0 then
            let struct (t, deltas, status) = hitData.[found]
            if priority = 1 then //we found a note
                if status.[k] <> HitStatus.HIT_ACCEPTED then
                    let isHoldHead = status.[k] <> HitStatus.HIT_REQUIRED
                    status.[k] <- HitStatus.HIT_ACCEPTED
                    deltas.[k] <- delta / rate
                    this._HandleEvent { Time = relativeTime; Column = k; Guts = Hit_ (deltas.[k], isHoldHead, false) }
                    if isHoldHead then internalHoldStates.[k] <- InternalHoldState.Holding
        elif internalHoldStates.[k] = InternalHoldState.MissedHead then internalHoldStates.[k] <- InternalHoldState.Dropped
                
    override this.HandleKeyUp (relativeTime: ChartTime, k: int) =
        this.HandlePassive relativeTime
        let now = firstNote + relativeTime
        while noteSeekActive < hitData.Length && InternalScore.offsetOf hitData.[noteSeekActive] < now - missWindow do 
            noteSeekActive <- noteSeekActive + 1
        let mutable i = noteSeekActive
        let mutable delta = missWindow
        let mutable found = -1
        let target = now + missWindow
        while i < hitData.Length && InternalScore.offsetOf hitData.[i] <= target do
            let struct (t, _, status) = hitData.[i]
            let d = now - t
            if status.[k] = HitStatus.RELEASE_REQUIRED then
                if Time.Abs delta > Time.Abs d then
                    found <- i
                    delta <- d
            elif status.[k] = HitStatus.HIT_HOLD_REQUIRED then
                i <- hitData.Length //stops the loop so we don't release an LN that isnt held yet
            i <- i + 1
        if found >= 0 then
            let struct (t, deltas, status) = hitData.[found]
            status.[k] <- HitStatus.RELEASE_ACCEPTED
            deltas.[k] <- delta / rate
            this._HandleEvent { Time = relativeTime; Column = k; Guts = Release_ (deltas.[k], false, false, internalHoldStates.[k] = InternalHoldState.Dropped) }
            internalHoldStates.[k] <- InternalHoldState.Nothing
        elif internalHoldStates.[k] = InternalHoldState.Holding then internalHoldStates.[k] <- InternalHoldState.Dropped
    
    abstract member HandleEvent : HitEvent<HitEventGutsInternal> -> HitEvent<HitEventGuts>
    member private this._HandleEvent ev =
        let ev = this.HandleEvent ev
        hitEvents.Add ev
        hitCallback ev
        healthBar.HandleEvent ev

(*
    Concrete implementations of health/accuracy systems
*)

type VibeGauge() =
    inherit HealthBarPointsSystem
        (
            "VG",
            0.5,
            false,
            ( fun j ->
                match j with
                | JudgementType.RIDICULOUS | JudgementType.MARVELLOUS -> 0.005
                | JudgementType.PERFECT -> 0.0025
                | JudgementType.GREAT -> 0.0
                | JudgementType.GOOD -> -0.05
                | JudgementType.BAD -> -0.2
                | JudgementType.MISS -> -0.1
                | _ -> failwith "impossible judgement type"
            ),
            0.0
        )

module ScoringHelpers =

    type HitWindows = (Time * JudgementType) list

    let rec window_func (windows: HitWindows) delta =
        let delta = Time.Abs delta
        match windows with
        | [] -> JudgementType.MISS
        | (w, j) :: xs ->
            if (delta < w) then j else window_func xs delta

    let dp_windows judge ridiculous =
        let perf_window = 45.0f<ms> / 6.0f * (10.0f - (judge |> float32))

        let windows: HitWindows =
            [ (perf_window * 0.5f, JudgementType.MARVELLOUS)
              (perf_window, JudgementType.PERFECT)
              (perf_window * 2.0f, JudgementType.GREAT)
              (min (perf_window * 3.0f) 135.0f<ms>, JudgementType.GOOD)
              (min (perf_window * 4.0f) 180.0f<ms>, JudgementType.BAD) ]

        window_func
            (if ridiculous
             then (perf_window * 0.25f, JudgementType.RIDICULOUS) :: windows
             else windows)

    let dp_windows_halved judge ridiculous =
        (fun (t: Time) -> t * 0.5f) >> (dp_windows judge ridiculous)

    let isComboBreaker = (function JudgementType.MISS | JudgementType.BAD | JudgementType.GOOD -> true | _ -> false)

type ScoreClassifierPlus(judge: int, enableRd: bool, healthBar, keys, replay, notes, rate) =
    inherit IScoreMetric
        (
            sprintf "SC+ (J%i)" judge,
            healthBar,
            180.0f<ms>,
            90.0f<ms>,
            keys, replay, notes, rate
        )

    let sc_curve (isRelease: bool) (delta: Time) =
        let delta = Time.Abs delta

        // 1.0 = 100%
        if delta >= 180.0f<ms> then -0.5
        else
            let delta = if isRelease then delta * 0.5f else delta
            let delta = float delta
            let scale = 6.0 / (10.0 - float judge)
            Math.Max(-1.0, (1.0 - Math.Pow(delta * scale, 2.8) * 0.0000056))

    override this.HandleEvent ev =
        { 
            Time = ev.Time
            Column = ev.Column
            Guts = 
                match ev.Guts with

                | Hit_ (delta, isHold, missed) ->
                    let judgement = ScoringHelpers.dp_windows judge enableRd delta
                    this.State.Add(sc_curve false delta, 1.0, judgement)
                    if ScoringHelpers.isComboBreaker judgement then this.State.BreakCombo() else this.State.IncrCombo()
                    Hit 
                        {|
                            Judgement = Some judgement
                            Missed = missed
                            Delta = delta
                            IsHold = isHold
                        |}

                | Release_ (delta, missed, overhold, dropped) ->
                    let judgement = ScoringHelpers.dp_windows_halved judge enableRd delta
                    this.State.Add(sc_curve true delta, 1.0, judgement)
                    if ScoringHelpers.isComboBreaker judgement then this.State.BreakCombo() else this.State.IncrCombo()
                    Release
                        {|
                            Judgement = Some judgement
                            Missed = missed
                            Delta = delta
                            Overhold = overhold
                            Dropped = dropped
                        |}
        }

type Wife3(judge: int, enableRd: bool, healthBar, keys, replay, notes, rate) =
    inherit IScoreMetric
        (
            sprintf "Wife3 (J%i)" judge,
            healthBar,
            180.0f<ms>,
            keys, replay, notes, rate
        )

    // lifted from https://github.com/etternagame/etterna/blob/0a7bd768cffd6f39a3d84d76964097e43011ce33/Themes/_fallback/Scripts/10%20Scores.lua#L606-L627
    let wife_curve (delta: Time) =
        let erf = 
            // was this really necessary
            let a1 =  0.254829592
            let a2 = -0.284496736
            let a3 =  1.421413741
            let a4 = -1.453152027
            let a5 =  1.061405429
            let p  =  0.3275911
            fun (x: float) ->
                let sign = if x < 0.0 then -1.0 else 1.0
                let x = Math.Abs x
                let t = 1.0 / (1.0 + p * x)
                let y = 1.0 - (((((a5 * t + a4) * t) + a3) * t + a2) * t + a1) * t * Math.Exp(-x * x)
                sign * y

        let delta = float delta |> Math.Abs

        let scale = (10.0 - float judge) / 6.0
        let miss_weight = -2.75
        let ridic = 5.0 * scale
        let boo_window = 180.0 * scale
        let ts_pow = 0.75
        let zero = 65.0 * Math.Pow(scale, ts_pow)
        let dev = 22.7 * Math.Pow(scale, ts_pow)

        if delta <= ridic then 1.0
        elif delta <= zero then erf ((zero - delta) / dev)
        elif delta <= boo_window then (delta - zero) * miss_weight / (boo_window - zero)
        else miss_weight

    override this.HandleEvent ev =
        { 
            Time = ev.Time
            Column = ev.Column
            Guts = 
                match ev.Guts with

                | Hit_ (delta, isHold, missed) ->
                    let judgement = ScoringHelpers.dp_windows judge enableRd delta
                    this.State.Add(wife_curve delta, 1.0, judgement)
                    if ScoringHelpers.isComboBreaker judgement then this.State.BreakCombo() else this.State.IncrCombo()
                    Hit 
                        {|
                            Judgement = Some judgement
                            Missed = missed
                            Delta = delta
                            IsHold = isHold
                        |}

                | Release_ (delta, missed, overhold, dropped) ->
                    if overhold || not missed then this.State.IncrCombo() else this.State.BreakCombo()
                    Release
                        {|
                            Judgement = None
                            Missed = missed
                            Delta = delta
                            Overhold = overhold
                            Dropped = dropped
                        |}
        }

type OsuMania(od: float32, healthBar, keys, replay, notes, rate) =
    inherit IScoreMetric
        (
            sprintf "osu!mania (OD%.1f)" od,
            healthBar,
            188.5f<ms> - od * 3.0f<ms>,
            keys, replay, notes, rate
        )

    let points =
        function
        | JudgementType.RIDICULOUS
        | JudgementType.MARVELLOUS
        | JudgementType.PERFECT -> 300.0
        | JudgementType.GREAT -> 200.0
        | JudgementType.GOOD -> 100.0
        | JudgementType.BAD -> 50.0
        | JudgementType.MISS -> 0.0
        | _ -> 0.0

    let headDeltas = Array.create keys 0.0f<ms>

    let judgementFunc (delta: Time) =
        let delta = Time.Abs delta
        if delta <= 16.5f<ms> then JudgementType.MARVELLOUS
        elif delta <= 64.5f<ms> - od * 3.0f<ms> then JudgementType.PERFECT
        elif delta <= 97.5f<ms> - od * 3.0f<ms> then JudgementType.GREAT
        elif delta <= 127.5f<ms> - od * 3.0f<ms> then JudgementType.GOOD
        elif delta <= 151.5f<ms> - od * 3.0f<ms> then JudgementType.BAD
        else JudgementType.MISS

    override this.HandleEvent ev =
        { 
            Time = ev.Time
            Column = ev.Column
            Guts = 
                match ev.Guts with

                | Hit_ (delta, isHold, missed) ->
                    if isHold then
                        headDeltas.[ev.Column] <- Time.Abs delta
                        if missed then this.State.BreakCombo() else this.State.IncrCombo()
                        Hit 
                            {|
                                Judgement = None
                                Missed = missed
                                Delta = delta
                                IsHold = true
                            |}
                    else
                        let judgement = judgementFunc delta
                        this.State.Add(points judgement, 300.0, judgement)
                        if judgement = JudgementType.MISS then this.State.BreakCombo() else this.State.IncrCombo()
                        Hit 
                            {|
                                Judgement = Some judgement
                                Missed = missed
                                Delta = delta
                                IsHold = true
                            |}

                | Release_ (delta, missed, overhold, dropped) ->
                    let headDelta = headDeltas.[ev.Column]
                    let absolute = Time.Abs delta * 0.5f

                    let judgement =
                        if
                            absolute < 16.5f<ms> * 1.2f &&
                            absolute + headDelta < 16.5f<ms> * 2.4f &&
                            (overhold || headDelta < 151.5f<ms> - od * 3.0f<ms>) &&
                            not dropped
                        then JudgementType.MARVELLOUS
                        elif
                            absolute < (64.5f<ms> - od * 3.0f<ms>) * 1.1f &&
                            absolute + headDelta < (64.5f<ms> - od * 3.0f<ms>) * 2.2f &&
                            (overhold || headDelta < 151.5f<ms> - od * 3.0f<ms>) &&
                            not dropped
                        then JudgementType.PERFECT
                        elif 
                            absolute < 97.5f<ms> - od * 3.0f<ms> &&
                            absolute + headDelta < (97.5f<ms> - od * 3.0f<ms>) * 2.0f &&
                            (overhold || headDelta < 151.5f<ms> - od * 3.0f<ms>)
                        then JudgementType.GREAT
                        elif
                            absolute < 127.5f<ms> - od * 3.0f<ms> &&
                            absolute + headDelta < (127.5f<ms> - od * 3.0f<ms>) * 2.0f &&
                            (overhold || headDelta < 151.5f<ms> - od * 3.0f<ms>)
                        then JudgementType.GOOD
                        elif
                            overhold || headDelta < 151.5f<ms> - od * 3.0f<ms>
                        then JudgementType.BAD
                        else JudgementType.MISS

                    this.State.Add(points judgement, 300.0, judgement)
                    if judgement = JudgementType.MISS then this.State.BreakCombo() else this.State.IncrCombo()
                    Release
                        {|
                            Judgement = Some judgement
                            Missed = missed
                            Delta = delta
                            Overhold = overhold
                            Dropped = dropped
                        |}
        }

module Metrics =
    
    type HPSystemConfig =
        | VG
        | OMHP of float
        | Custom of unit
        override this.ToString() =
            match this with
            | VG -> "VG"
            | OMHP hp -> "osu!mania (HP" + (string hp) + ")"
            | _ -> "unknown"

    let createHealthBar (config: HPSystemConfig) : IHealthBarSystem =
        match config with
        | VG -> VibeGauge() :> IHealthBarSystem
        | _ -> failwith "nyi"

    type AccuracySystemConfig =
        | SC of judge: int * ridiculous: bool
        | SCPlus of judge: int * ridiculous: bool
        | Wife of judge: int * ridiculous: bool
        | OM of od: float32
        | Custom of unit
        override this.ToString() =
            match this with
            | SC (judge, rd) -> "SC (J" + (string judge) + ")"
            | SCPlus (judge, rd) -> "SC+ (J" + (string judge) + ")"
            | Wife (judge, rd) -> "Wife3 (J" + (string judge) + ")"
            | OM od -> "osu!mania (OD" + (string od) + ")"
            | _ -> "unknown"
    
    let createScoreMetric accConfig hpConfig keys replay notes rate : IScoreMetric =
        let hp = createHealthBar hpConfig
        match accConfig with
        | SC (judge, rd)
        | SCPlus (judge, rd) -> ScoreClassifierPlus(judge, rd, hp, keys, replay, notes, rate) :> IScoreMetric
        | Wife (judge, rd) -> Wife3(judge, rd, hp, keys, replay, notes, rate) :> IScoreMetric
        | OM od -> OsuMania(od, hp, keys, replay, notes, rate) :> IScoreMetric
        | _ -> failwith "nyi"