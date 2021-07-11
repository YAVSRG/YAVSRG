namespace Prelude.Scoring

open System
open Prelude.Common
open Prelude.Charts.Interlude

// user-facing flags of "judgements"; how well you hit notes
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

    abstract member HandleMineExplosion : unit -> unit
    abstract member HandleMineDodged : unit -> unit
    abstract member HandleHoldDropped : unit -> unit
    abstract member HandleHoldOK : unit -> unit

    abstract member HandleNote : JudgementType * Time -> unit
    abstract member HandleRelease : JudgementType * Time -> unit
        
    member this.Failed = if onlyFailAtEnd then state.CurrentlyFailed else state.HasFailed
    member this.Name = name
    member this.Format() = sprintf "%.2f%%" state.Health

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

    override this.HandleMineExplosion() = this.ChangeHP(points_func JudgementType.NG)
    override this.HandleMineDodged() = ()
    override this.HandleHoldDropped() = ()
    override this.HandleHoldOK() = ()

    override this.HandleNote (j, _: Time) = this.ChangeHP(points_func j)
    override this.HandleRelease (j, _: Time) = this.ChangeHP(points_func j)

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
type ScoreMetric
    (
        name: string,
        healthBar: IHealthBarSystem,
        missWindow: Time,
        keys: int,
        replayProvider: IReplayProvider,
        notes: TimeData<NoteRow>,
        rate: float32
    ) =
    inherit ReplayConsumer(keys, replayProvider)

    let missWindow = missWindow * rate
    // some score systems can modify this internal data, to remove the need to time releases for example
    let hitdata = InternalScore.createDefault missWindow keys notes
    let mutable noteSeek = 0

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
    member this.FormatAccuracy() =
        let value = (state.PointsScored / state.MaxPointsScored)
        (if Double.IsNaN value then 1.0 else value) |> sprintf "%.2f%%"
    member this.HP = healthBar
    member this.MissWindow = missWindow
    member this.State: AccuracySystemState = state
    member this.HitData: InternalScoreData = hitdata

    member this.Update(time: Time) =
        this.PollReplay time // calls HandleKeyDown and HandleKeyUp appropriately
        this.HandlePassive time

    member private this.HandlePassive(now) =
        let target = now - missWindow
        while noteSeek < hitdata.Length && InternalScore.offsetOf hitdata.[noteSeek] <= target do
            let struct (t, deltas, status) = hitdata.[noteSeek]
            for k = 0 to (keys - 1) do
                if status.[k] = HitStatus.NEEDS_TO_BE_HIT then
                    this._HandleNote(JudgementType.MISS, deltas.[k])
                elif status.[k] = HitStatus.NEEDS_TO_BE_RELEASED then
                    this._HandleRelease(JudgementType.MISS, deltas.[k])
                elif status.[k] = HitStatus.NEEDS_TO_BE_DODGED then
                    this._HandleMineDodged()
                elif status.[k] = HitStatus.NEEDS_TO_BE_RELEASED then
                    this._HandleHoldOK()
            noteSeek <- noteSeek + 1
        // missing: checks key down as mine passes receptors, key up as hold middle passes receptors
        // algorithm must "forget" what it finds if
            // for mines, nothing
            // for lns, if we see a hold head or tail within misswindow

    override this.HandleKeyDown(now, k) =
        let mutable i = noteSeek
        let mutable delta = missWindow
        let mutable found = -1
        let mutable priority = -1
        let target = now + missWindow
        while i < hitdata.Length && InternalScore.offsetOf hitdata.[noteSeek] <= now - missWindow do i <- i + 1
        while i < hitdata.Length && InternalScore.offsetOf hitdata.[noteSeek] <= target do
            let struct (t, _, status) = hitdata.[noteSeek]
            let d = now - t
            if status.[k] = HitStatus.NEEDS_TO_BE_HIT then
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
                status.[k] <- HitStatus.HAS_BEEN_HIT
                deltas.[k] <- delta
                this._HandleNote(this.JudgementFunc (false, deltas.[k]), deltas.[k])
            else //priority = 0, we found a mine
                status.[k] <- HitStatus.HAS_NOT_BEEN_DODGED
                this._HandleMineExplosion()
                
    override this.HandleKeyUp(now, k) =
        let mutable i = noteSeek
        let mutable delta = missWindow
        let mutable found = -1
        let mutable priority = -1
        let target = now + missWindow
        while i < hitdata.Length && InternalScore.offsetOf hitdata.[noteSeek] <= now - missWindow do i <- i + 1
        while i < hitdata.Length && InternalScore.offsetOf hitdata.[noteSeek] <= target do
            let struct (t, _, status) = hitdata.[noteSeek]
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
            elif status.[k] = HitStatus.NEEDS_TO_BE_HIT then
                i <- hitdata.Length //stops the loop so we don't release an LN that isnt held yet
            i <- i + 1
        if found >= 0 then
            let struct (t, deltas, status) = hitdata.[found]
            if priority = 1 then //we found a release
                delta <- delta / rate
                status.[k] <- HitStatus.HAS_BEEN_RELEASED
                deltas.[k] <- delta
                this._HandleRelease(this.JudgementFunc (true, deltas.[k]), deltas.[k])
            else //priority = 0, we released a hold body
                status.[k] <- HitStatus.HAS_NOT_BEEN_HELD
                this._HandleHoldDropped()

    abstract member JudgementFunc : bool * Time -> JudgementType
    abstract member PointsFunc : bool * JudgementType * Time -> float
    
    abstract member HandleMineExplosion : unit -> unit
    member private this._HandleMineExplosion() =
        this.HandleMineExplosion()
        healthBar.HandleMineExplosion()
        
    abstract member HandleMineDodged : unit -> unit
    member private this._HandleMineDodged() =
        this.HandleMineDodged()
        healthBar.HandleMineDodged()

    abstract member HandleHoldDropped : unit -> unit
    member private this._HandleHoldDropped() =
        this.HandleHoldDropped()
        healthBar.HandleHoldDropped()
    
    abstract member HandleHoldOK : unit -> unit
    member private this._HandleHoldOK() =
        this.HandleHoldOK()
        healthBar.HandleHoldOK()

    abstract member HandleNote : JudgementType * Time -> unit
    member private this._HandleNote(judge, time) =
        this.HandleNote(judge, time)
        healthBar.HandleNote(judge, time)

    abstract member HandleRelease : JudgementType * Time -> unit
    member private this._HandleRelease(judge, time) =
        this.HandleRelease(judge, time)
        healthBar.HandleRelease(judge, time)

// simple constructor for "typical" score systems. for additional customisation of behaviour, override ScoreMetric instead
type RegularScoreMetric
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
    inherit ScoreMetric(name, healthBar, missWindow, keys, replayProvider, notes, rate)

    override this.HandleMineExplosion() = this.State.Add JudgementType.NG
    override this.HandleMineDodged() = this.State.Add JudgementType.OK
    override this.HandleHoldDropped() = this.State.Add JudgementType.NG
    override this.HandleHoldOK() = this.State.Add JudgementType.OK

    override this.HandleNote(j, delta) =
        if comboFunc j then this.State.BreakCombo()
        this.State.Add (this.PointsFunc(false, j, delta), 1.0, j)
    override this.HandleRelease(j, delta) =
        if comboFunc j then this.State.BreakCombo()
        this.State.Add (this.PointsFunc(false, j, delta), 1.0, j)

    override this.JudgementFunc(isRelease, delta) = judgementFunc isRelease delta
    override this.PointsFunc(isRelease, judgement, delta) = pointsFunc isRelease judgement delta