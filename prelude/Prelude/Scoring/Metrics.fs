namespace Prelude.Scoring

open System
open Prelude.Common
open Prelude.Charts.Interlude

// user-facing flags of "judgements", how well you hit notes
type JudgementType =
    | RIDICULOUS = 0
    | MARVELLOUS = 1
    | PERFECT = 2
    | OK = 3
    | GREAT = 4
    | GOOD = 5
    | BAD = 6
    | NG = 7
    | MISS = 8
module JudgementType = let count = 9

type HitEventGuts =
    | Hit of judgement: JudgementType * delta: Time * isHoldHead: bool
    | Release of judgement: JudgementType * delta: Time
    | Mine of dodged: bool
    | Hold of held: bool

type HitEvent =
    {
        Column: int
        Guts: HitEventGuts
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
    abstract member HandleEvent : HitEvent -> unit
        
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
        | Hit (j, delta, _)
        | Release (j, delta) -> this.ChangeHP(points_func j)
        | Mine good -> if not good then this.ChangeHP(points_func JudgementType.NG)
        | Hold good -> ()

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

[<AbstractClass>]
type IScoreMetric
    (
        name: string,
        healthBar: IHealthBarSystem,
        rawMissWindow: Time,
        keys: int,
        replayProvider: IReplayProvider,
        notes: TimeData<NoteRow>,
        rate: float32
    ) =
    inherit ReplayConsumer(keys, replayProvider)

    let missWindow = rawMissWindow * rate
    // some score systems can modify this internal data, to remove the need to time releases for example
    let hitdata = InternalScore.createDefault missWindow keys notes
    // having two seekers improves performance when feeding scores rather than playing live
    let mutable noteSeekPassive = 0
    let mutable noteSeekActive = 0

    let mutable hitCallback = fun ev -> ()

    let state =
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
        let v = state.PointsScored / state.MaxPointsScored
        if Double.IsNaN v then 1.0 else v
    member this.FormatAccuracy() = sprintf "%.2f%%" (this.Value * 100.0)
    member this.HP = healthBar
    member this.MissWindow = rawMissWindow
    member this.ScaledMissWindow = missWindow
    member this.State: AccuracySystemState = state
    member this.HitData: InternalScoreData = hitdata

    member this.SetHitCallback (func: HitEvent -> unit) = hitCallback <- func

    member this.Finished = noteSeekPassive = hitdata.Length

    // correctness guaranteed up to the time you update, no matter how you update
    // call Update with infinityf to do a correct feeding of the whole replay
    member this.Update(time: Time) =
        this.PollReplay time // calls HandleKeyDown and HandleKeyUp appropriately
        this.HandlePassive time

    member private this.HandlePassive(now) =
        let target = now - missWindow
        while noteSeekPassive < hitdata.Length && InternalScore.offsetOf hitdata.[noteSeekPassive] <= target do
            let struct (t, deltas, status) = hitdata.[noteSeekPassive]
            for k = 0 to (keys - 1) do
                if status.[k] = HitStatus.NEEDS_TO_BE_HIT then this._HandleEvent { Column = k; Guts = Hit (JudgementType.MISS, deltas.[k], false) }
                elif status.[k] = HitStatus.NEEDS_TO_BE_HIT_AND_HELD then this._HandleEvent { Column = k; Guts = Hit (JudgementType.MISS, deltas.[k], true) }
                elif status.[k] = HitStatus.NEEDS_TO_BE_RELEASED then this._HandleEvent { Column = k; Guts = Release (JudgementType.MISS, deltas.[k]) }
                elif status.[k] = HitStatus.NEEDS_TO_BE_DODGED then this._HandleEvent { Column = k; Guts = Mine true }
                elif status.[k] = HitStatus.NEEDS_TO_BE_HELD then this._HandleEvent { Column = k; Guts = Hold true }
            noteSeekPassive <- noteSeekPassive + 1
        // missing: checks key down as mine passes receptors, key up as hold middle passes receptors
        // algorithm must "forget" what it finds if
            // for mines, nothing
            // for lns, if we see a hold head or tail within misswindow

    override this.HandleKeyDown(now, k) =
        while noteSeekActive < hitdata.Length && InternalScore.offsetOf hitdata.[noteSeekActive] < now - missWindow do 
            noteSeekActive <- noteSeekActive + 1
        let mutable i = noteSeekActive
        let mutable delta = missWindow
        let mutable found = -1
        let mutable priority = -1
        let target = now + missWindow
        while i < hitdata.Length && InternalScore.offsetOf hitdata.[i] <= target do
            let struct (t, _, status) = hitdata.[i]
            let d = now - t
            if status.[k] = HitStatus.NEEDS_TO_BE_HIT || status.[k] = HitStatus.NEEDS_TO_BE_HIT_AND_HELD then
                if (Time.Abs delta > Time.Abs d) || priority < 1 then
                    priority <- 1
                    found <- i
                    delta <- d
            elif status.[k] = HitStatus.NEEDS_TO_BE_DODGED then
                if (Time.Abs delta > Time.Abs d) || priority < 0 then
                    priority <- 0
                    found <- i
                    delta <- d
            i <- i + 1
        if found >= 0 then
            let struct (t, deltas, status) = hitdata.[found]
            if priority = 1 then //we found a note
                delta <- delta / rate
                let isHoldHead = status.[k] <> HitStatus.NEEDS_TO_BE_HIT
                status.[k] <- HitStatus.HAS_BEEN_HIT
                deltas.[k] <- delta
                this._HandleEvent { Column = k; Guts = Hit (this.JudgementFunc (false, Time.Abs deltas.[k]), deltas.[k], isHoldHead) }
            else //priority = 0, we found a mine
                status.[k] <- HitStatus.HAS_NOT_BEEN_DODGED
                this._HandleEvent { Column = k; Guts = Mine false }
                
    override this.HandleKeyUp(now, k) =
        while noteSeekActive < hitdata.Length && InternalScore.offsetOf hitdata.[noteSeekActive] < now - missWindow do 
            noteSeekActive <- noteSeekActive + 1
        let mutable i = noteSeekActive
        let mutable delta = missWindow
        let mutable found = -1
        let mutable priority = -1
        let target = now + missWindow
        while i < hitdata.Length && InternalScore.offsetOf hitdata.[i] <= target do
            let struct (t, _, status) = hitdata.[i]
            let d = now - t
            if status.[k] = HitStatus.NEEDS_TO_BE_RELEASED then
                if (Time.Abs delta > Time.Abs d) || priority < 1 then
                    priority <- 1
                    found <- i
                    delta <- d
            elif status.[k] = HitStatus.NEEDS_TO_BE_HELD then
                if (Time.Abs delta > Time.Abs d) || priority < 0 then
                    priority <- 0
                    found <- i
                    delta <- d
            elif status.[k] = HitStatus.NEEDS_TO_BE_HIT_AND_HELD then
                i <- hitdata.Length //stops the loop so we don't release an LN that isnt held yet
            i <- i + 1
        if found >= 0 then
            let struct (t, deltas, status) = hitdata.[found]
            if priority = 1 then //we found a release
                delta <- delta / rate
                status.[k] <- HitStatus.HAS_BEEN_RELEASED
                deltas.[k] <- delta
                this._HandleEvent { Column = k; Guts = Release (this.JudgementFunc (false, Time.Abs deltas.[k]), deltas.[k]) }
            else //priority = 0, we released a hold body
                status.[k] <- HitStatus.HAS_NOT_BEEN_HELD
                this._HandleEvent { Column = k; Guts = Hold false }

    abstract member JudgementFunc : bool * Time -> JudgementType
    
    abstract member HandleEvent : HitEvent -> unit
    member private this._HandleEvent ev =
        hitCallback ev
        this.HandleEvent ev
        healthBar.HandleEvent ev

// simple constructor for "typical" score systems. for additional customisation of behaviour, override ScoreMetric instead
type ScorePointsMetric
    (
        name: string,
        healthBar: IHealthBarSystem,
        missWindow: Time,
        keys: int,
        replayProvider: IReplayProvider,
        notes: TimeData<NoteRow>,
        rate: float32,

        comboFunc,
        judgementFunc,
        pointsFunc
    ) =
    inherit IScoreMetric(name, healthBar, missWindow, keys, replayProvider, notes, rate)

    override this.HandleEvent ev =
        match ev.Guts with
        | Hit (j, delta, _) ->
            if comboFunc j then this.State.BreakCombo() else this.State.IncrCombo()
            this.State.Add (pointsFunc false j (Time.Abs delta), 1.0, j)
        | Release (j, delta) ->
            if comboFunc j then this.State.BreakCombo() else this.State.IncrCombo()
            this.State.Add (pointsFunc true j (Time.Abs delta), 1.0, j)
        | Mine good
        | Hold good ->
            if good then this.State.Add JudgementType.OK
            else this.State.Add JudgementType.NG; this.State.BreakCombo()

    override this.JudgementFunc(isRelease, delta) = judgementFunc isRelease delta

(*
    Concrete implementations of health/accuracy systems
*)

type VibeGauge() =
    inherit HealthBarPointsSystem
        (
            "VG",
            0.5,
            false,
            (fun j ->
                match j with
                | JudgementType.RIDICULOUS | JudgementType.MARVELLOUS -> 0.005
                | JudgementType.PERFECT -> 0.0025
                | JudgementType.GREAT -> 0.0
                | JudgementType.GOOD -> -0.05
                | JudgementType.BAD -> -0.2
                | JudgementType.MISS -> -0.1
                | JudgementType.OK -> 0.0
                | JudgementType.NG -> -0.1
                | _ -> failwith "impossible judgement type"),
            0.0
        )

module ScoringHelpers =

    type HitWindows = (Time * JudgementType) list

    let rec window_func (windows: HitWindows) delta =
        assert (delta >= 0.0f<ms>)
        match windows with
        | [] -> JudgementType.MISS
        | (w, j) :: xs ->
            if (delta < w) then j else window_func xs delta

    let dp_windows judge ridiculous _ =
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

    let sc_windows judge ridiculous =
        let win = dp_windows judge ridiculous false
        fun isRelease delta ->
            if isRelease then win (delta * 0.5f) else win delta
    
    let sc_curve (judge: int) (isRelease: bool) (judgement: JudgementType) (delta: Time) =
        assert (delta >= 0.0f<ms>)
        let delta = if isRelease then delta * 0.5f else delta
        if delta >= 180.0f<ms> then -0.5
        else
            let delta = float delta
            let scale = 6.0 / (10.0 - float judge)
            Math.Max(-1.0, (1.0 - Math.Pow(delta * scale, 2.8) * 0.0000056))

    // now finally ported to wife3
    let wife_curve (judge: int) (_: bool) (_: JudgementType) (delta: Time) =
        // lifted from https://github.com/etternagame/etterna/blob/0a7bd768cffd6f39a3d84d76964097e43011ce33/Themes/_fallback/Scripts/10%20Scores.lua#L606-L627
        let erf = 
            // you really had to put erf in this one didnt you
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

        assert (delta >= 0.0f<ms>)
        let delta = float delta

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

type ScoreClassifier(judge: int, enableRd: bool, healthBar, keys, replay, notes, rate) =
    inherit ScorePointsMetric
        (
            sprintf "SC+ (J%i)" judge,
            healthBar,
            180.0f<ms>,
            keys, replay, notes, rate,
            (function JudgementType.MISS | JudgementType.BAD | JudgementType.GOOD -> true | _ -> false),
            ScoringHelpers.sc_windows judge enableRd,
            ScoringHelpers.sc_curve judge
        )

type Wife3(judge: int, enableRd: bool, healthBar, keys, replay, notes, rate) as this =
    inherit ScorePointsMetric
        (
            sprintf "Wife3 (J%i)" judge,
            healthBar,
            180.0f<ms>,
            keys, replay, notes, rate,
            (function JudgementType.MISS | JudgementType.BAD | JudgementType.GOOD -> true | _ -> false),
            ScoringHelpers.dp_windows judge enableRd,
            ScoringHelpers.wife_curve judge
        )
    // disable release timing
    do
        for struct (time, deltas, status) in this.HitData do
            for k = 0 to (keys - 1) do
                if status.[k] = HitStatus.NEEDS_TO_BE_RELEASED then status.[k] <- HitStatus.NOTHING

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
        | SCPlus (judge, rd) -> ScoreClassifier(judge, rd, hp, keys, replay, notes, rate) :> IScoreMetric
        | Wife (judge, rd) -> Wife3(judge, rd, hp, keys, replay, notes, rate) :> IScoreMetric
        | _ -> failwith "nyi"