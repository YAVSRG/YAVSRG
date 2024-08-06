namespace Prelude.Gameplay

open System
open Prelude
open Prelude.Charts

[<AbstractClass>]
type ScoreProcessorBase(ruleset: Ruleset, keys: int, replay: IReplayProvider, notes: TimeArray<NoteRow>, rate: float32) =
    inherit ReplayConsumer(keys, replay)

    let first_note = (TimeArray.first notes).Value.Time
    let last_note = (TimeArray.last notes).Value.Time
    let duration = last_note - first_note
    let miss_window = ruleset.Accuracy.MissWindow * rate

    // having two seekers improves performance when processing entire scores
    let mutable note_seek_passive = 0
    let mutable note_seek_active = 0

    let hold_states = Array.create keys (Nothing, -1)

    let snapshots = ResizeArray<ScoreMetricSnapshot>()
    let hit_data = InternalScore.create_gameplay ruleset.Accuracy.MissWindow keys notes
    let hit_events = ResizeArray<HitEvent<HitEventGuts>>()

    let on_hit_ev = Event<HitEvent<HitEventGuts>>()
    let on_hit = on_hit_ev.Publish

    //let osu_window_behaviour =
    //    match ruleset.Accuracy.HoldNoteBehaviour with
    //    | HoldNoteBehaviour.Osu _ -> true
    //    | _ -> false

    member this.OnHit = on_hit

    member val State : AccuracySystemState =
        {
            Judgements = Array.zeroCreate ruleset.Judgements.Length
            PointsScored = 0.0
            MaxPointsScored = 0.0
            CurrentCombo = 0
            BestCombo = 0
            ComboBreaks = 0
            MaxPossibleCombo = 0
            Sxx = 0.0f<ms * ms>
            Sx = 0.0f<ms>
            N = 0.0f
        }

    member this.Name = ruleset.Name

    member this.Accuracy =
        let v = this.State.PointsScored / this.State.MaxPointsScored
        if Double.IsNaN v then 1.0 else v

    member this.FormatAccuracy() = format_accuracy this.Accuracy
    member this.MissWindow = ruleset.Accuracy.MissWindow
    member this.ScaledMissWindow = miss_window
    member this.Ruleset = ruleset
    member this.EnumerateRecentInputs() = replay.EnumerateRecentEvents()

    member this.HoldState (index: int) (k: int) =
        let state, i = hold_states.[k]

        if i = index then
            match state with
            | Nothing -> HoldState.Released
            | Holding -> HoldState.Holding
            | Dropped -> HoldState.Dropped
            | MissedHead
            | MissedHeadThenHeld -> HoldState.MissedHead
        elif i > index then
            let struct (_, _, flags) = hit_data.[index]

            if flags.[k] <> HitStatus.HIT_HOLD_REQUIRED then
                HoldState.Released
            else
                HoldState.MissedHead
        else
            HoldState.InTheFuture

    member this.IsNoteHit (index: int) (k: int) =
        let struct (_, _, flags) = hit_data.[index]
        flags.[k] = HitStatus.HIT_ACCEPTED

    member this.HitData = hit_data // todo: stop exposing this

    member this.Finished = note_seek_passive = hit_data.Length

    member this.HitEvents = hit_events.AsReadOnly()
    member this.Snapshots = snapshots.AsReadOnly()

    // correctness guaranteed up to the time you update, no matter how you update
    // call Update with Time.infinity to do a correct feeding of the whole replay
    member this.Update(chart_time: ChartTime) =
        this.PollReplay chart_time // calls HandleKeyDown and HandleKeyUp appropriately
        this.HandlePassive chart_time

    member private this.UpdateStateSnapshots(chart_time: ChartTime) =
        let snapshot_target_count =
            if chart_time > duration then 
                ScoreMetricSnapshot.COUNT
            else
                (float32 ScoreMetricSnapshot.COUNT * chart_time) / duration
                |> ceil
                |> int
                |> max 0
                |> min ScoreMetricSnapshot.COUNT

        while snapshots.Count < snapshot_target_count do
            snapshots.Add
                {
                    Time = float32 (snapshots.Count + 1) / float32 ScoreMetricSnapshot.COUNT * duration
                    PointsScored = this.State.PointsScored
                    MaxPointsScored = this.State.MaxPointsScored
                    Combo = this.State.CurrentCombo
                    Lamp = Lamp.calculate ruleset.Grading.Lamps this.State
                    Mean = this.State.Mean
                    StandardDeviation = this.State.StandardDeviation
                    Judgements = this.State.Judgements |> Array.copy
                }

    member private this.HandlePassive(chart_time: ChartTime) =
        let now = first_note + chart_time
        let target = now - miss_window

        while note_seek_passive < hit_data.Length
              && InternalScore.offsetOf hit_data.[note_seek_passive] <= target do
            let struct (t, deltas, status) = hit_data.[note_seek_passive]

            for k = 0 to (keys - 1) do

                if status.[k] = HitStatus.HIT_REQUIRED then
                    this._HandleEvent
                        {
                            Index = note_seek_passive
                            Time = t - first_note + miss_window
                            Column = k
                            Guts = Hit_(deltas.[k], false, true)
                        }

                elif status.[k] = HitStatus.HIT_HOLD_REQUIRED then
                    hold_states.[k] <- MissedHead, note_seek_passive

                    this._HandleEvent
                        {
                            Index = note_seek_passive
                            Time = t - first_note + miss_window
                            Column = k
                            Guts = Hit_(deltas.[k], true, true)
                        }

                elif status.[k] = HitStatus.RELEASE_REQUIRED then
                    let overhold =
                        match hold_states.[k] with
                        | Dropped, i
                        | Holding, i when i <= note_seek_passive -> Bitmask.has_key k this.KeyState
                        | _ -> false

                    let dropped =
                        match hold_states.[k] with
                        | Dropped, _
                        | MissedHeadThenHeld, _
                        | MissedHead, _ -> true
                        | _ -> false

                    let missed_head =
                        match hold_states.[k] with
                        | MissedHeadThenHeld, _ -> true
                        | _ -> false

                    this._HandleEvent
                        {
                            Index = note_seek_passive
                            Time = t - first_note + miss_window
                            Column = k
                            Guts = Release_(deltas.[k], true, overhold, dropped, missed_head)
                        }

                    match hold_states.[k] with
                    | _, i when i < note_seek_passive -> hold_states.[k] <- Nothing, note_seek_passive
                    | _ -> ()

            note_seek_passive <- note_seek_passive + 1

        this.UpdateStateSnapshots(chart_time)

    override this.HandleKeyDown(chart_time: ChartTime, k: int) =
        this.HandlePassive chart_time
        let now = first_note + chart_time

        while note_seek_active < hit_data.Length
              && InternalScore.offsetOf hit_data.[note_seek_active] < now - miss_window do
            note_seek_active <- note_seek_active + 1

        let mutable i = note_seek_active
        let mutable cbrush_absorb_delta = miss_window
        let mutable matching_note_index = -1
        let mutable matching_note_delta = miss_window
        let target = now + miss_window

        while i < hit_data.Length && InternalScore.offsetOf hit_data.[i] <= target do
            let struct (t, deltas, status) = hit_data.[i]
            let delta = now - t

            // Find unhit note that is closer than the current candidate
            if (status.[k] = HitStatus.HIT_REQUIRED || status.[k] = HitStatus.HIT_HOLD_REQUIRED) then
                if Time.abs matching_note_delta > Time.abs delta then
                    matching_note_index <- i
                    matching_note_delta <- delta

                // If new candidate is within cbrush window, stop looking resulting in earliest match being used
                // Otherwise keep looking for something closer and allow this note to be missed
                if Time.abs matching_note_delta < ruleset.Accuracy.CbrushWindow then
                    i <- hit_data.Length
            // Find hit note that got hit earlier than the cbrush window, and track how close it is
            elif
                status.[k] = HitStatus.HIT_ACCEPTED
                && deltas.[k] < -ruleset.Accuracy.CbrushWindow
            then
                if Time.abs cbrush_absorb_delta > Time.abs delta then
                    cbrush_absorb_delta <- delta

            i <- i + 1

        if matching_note_index >= 0 then
            let struct (_, deltas, status) = hit_data.[matching_note_index]
            // If fumbled note detected is closer than hit candidate, swallow the hit
            if Time.abs cbrush_absorb_delta >= Time.abs matching_note_delta then
                let is_hold_head = status.[k] <> HitStatus.HIT_REQUIRED
                status.[k] <- HitStatus.HIT_ACCEPTED
                deltas.[k] <- matching_note_delta / rate

                this._HandleEvent
                    {
                        Index = matching_note_index
                        Time = chart_time
                        Column = k
                        Guts = Hit_(deltas.[k], is_hold_head, false)
                    }
                // Begin tracking if it's a hold note
                if is_hold_head then
                    hold_states.[k] <- Holding, matching_note_index

        else // If no note to hit, but a hold note head was missed, pressing key marks it dropped instead
            hold_states.[k] <-
                match hold_states.[k] with
                | MissedHead, i -> MissedHeadThenHeld, i
                | x -> x

    override this.HandleKeyUp(chart_time: ChartTime, k: int) =
        this.HandlePassive chart_time
        let now = first_note + chart_time

        match hold_states.[k] with
        | Holding, head_index
        | Dropped, head_index
        | MissedHeadThenHeld, head_index ->

            let mutable i = head_index
            let mutable delta = miss_window
            let mutable found = -1
            let target = now + miss_window

            while i < hit_data.Length && InternalScore.offsetOf hit_data.[i] <= target do
                let struct (t, _, status) = hit_data.[i]
                let d = now - t

                if status.[k] = HitStatus.RELEASE_REQUIRED then
                    // Get the first unreleased hold tail we see, after the head of the hold we're tracking
                    found <- i
                    delta <- d
                    i <- hit_data.Length

                i <- i + 1

            if found >= 0 then
                let struct (_, deltas, status) = hit_data.[found]
                status.[k] <- HitStatus.RELEASE_ACCEPTED
                deltas.[k] <- delta / rate

                this._HandleEvent
                    {
                        Index = found
                        Time = chart_time
                        Column = k
                        Guts =
                            Release_(
                                deltas.[k],
                                false,
                                false,
                                fst hold_states.[k] = Dropped || fst hold_states.[k] = MissedHeadThenHeld,
                                fst hold_states.[k] = MissedHeadThenHeld
                            )
                    }

                hold_states.[k] <- Nothing, head_index
            else // If we released but too early (no sign of the tail within range) make the long note dropped
                hold_states.[k] <-
                    match hold_states.[k] with
                    | Holding, i -> Dropped, i
                    | x -> x

                match ruleset.Accuracy.HoldNoteBehaviour with
                | HoldNoteBehaviour.Osu _ -> this.State.BreakCombo(false)
                | _ -> ()
        | MissedHead, _
        | Nothing, _ -> ()

    abstract member HandleEvent: HitEvent<HitEventGutsInternal> -> HitEvent<HitEventGuts>

    member private this._HandleEvent ev =
        let ev = this.HandleEvent ev
        hit_events.Add ev
        on_hit_ev.Trigger ev

module Helpers =

    let ms_to_judgement (default_judgement: JudgementId) (gates: (Time * JudgementId) list) (delta: Time) : JudgementId =
        let rec loop gates =
            match gates with
            | [] -> default_judgement
            | (w, j) :: xs -> if delta < w then j else loop xs

        loop gates

    let judgement_to_points (conf: Ruleset) (delta: Time) (judge: JudgementId) : float =
        match conf.Accuracy.Points with
        | AccuracyPoints.WifeCurve j -> RulesetUtils.wife_curve j delta
        | AccuracyPoints.Weights(maxweight, weights) -> weights.[judge] / maxweight

type ScoreProcessor(ruleset: Ruleset, keys: int, replay: IReplayProvider, notes: TimeArray<NoteRow>, rate: float32) =
    inherit ScoreProcessorBase(ruleset, keys, replay, notes, rate)

    let head_judgements = Array.create keys ruleset.DefaultJudgement
    let head_deltas = Array.create keys ruleset.Accuracy.MissWindow

    let judgement_to_points = Helpers.judgement_to_points ruleset

    let ms_to_judgement = Helpers.ms_to_judgement ruleset.DefaultJudgement ruleset.Accuracy.Timegates

    member this.HandleTap (delta: Time, is_miss: bool) =

        if not is_miss then
            this.State.AddDelta delta
            
        match ruleset.Accuracy.HoldNoteBehaviour with 
        | HoldNoteBehaviour.OnlyJudgeReleases _ ->
            if is_miss then this.State.BreakCombo true else this.State.IncrCombo()
            Hit
                {|
                    Judgement = None
                    Missed = is_miss
                    Delta = delta
                    IsHold = false
                |}
        | _ ->
            let judgement = ms_to_judgement delta

            this.State.Add(judgement_to_points delta judgement, 1.0, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                this.State.BreakCombo true
            else
                this.State.IncrCombo()

            Hit
                {|
                    Judgement = Some judgement
                    Missed = is_miss
                    Delta = delta
                    IsHold = false
                |}

    member this.HandleHoldHead (column: int, delta: Time, is_miss: bool) =
        let judgement = ms_to_judgement delta

        if not is_miss then
            this.State.AddDelta delta

        head_judgements.[column] <- judgement
        head_deltas.[column] <- delta

        match ruleset.Accuracy.HoldNoteBehaviour with
        | HoldNoteBehaviour.JustBreakCombo
        | HoldNoteBehaviour.JudgeReleases _ ->
            this.State.Add(judgement_to_points delta judgement, 1.0, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                this.State.BreakCombo true
            else
                this.State.IncrCombo()

            Hit
                {|
                    Judgement = Some judgement
                    Missed = is_miss
                    Delta = delta
                    IsHold = true
                |}

        | HoldNoteBehaviour.Osu _
        | HoldNoteBehaviour.Normal _
        | HoldNoteBehaviour.OnlyJudgeReleases _ ->
            Hit
                {|
                    Judgement = None
                    Missed = is_miss
                    Delta = delta
                    IsHold = true
                |}

    member this.HandleHoldTail (column: int, delta: Time, is_miss: bool, is_overhold: bool, is_dropped: bool, is_head_missed: bool) =
        match ruleset.Accuracy.HoldNoteBehaviour with
        | HoldNoteBehaviour.Osu windows ->
            let judgement =
                RulesetUtils.osu_ln_judgement windows head_deltas.[column] delta is_overhold is_dropped

            this.State.Add(judgement_to_points delta judgement, 1.0, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo || is_head_missed then
                this.State.BreakCombo true
            else
                this.State.IncrCombo()

            Release
                {|
                    Judgement = Some judgement
                    Missed = is_miss
                    Delta = delta
                    Overhold = is_overhold
                    Dropped = is_dropped
                |}

        | HoldNoteBehaviour.JustBreakCombo ->
            if (not is_overhold) && (is_miss || is_dropped) then
                this.State.BreakCombo true
            else
                this.State.IncrCombo()

            Release
                {|
                    Judgement = None
                    Missed = is_miss
                    Delta = delta
                    Overhold = is_overhold
                    Dropped = is_dropped
                |}

        | HoldNoteBehaviour.JudgeReleases data ->
            let judgement = Helpers.ms_to_judgement ruleset.DefaultJudgement data.Timegates delta
            this.State.Add(judgement_to_points delta judgement, 1.0, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                this.State.BreakCombo true
            else
                this.State.IncrCombo()

            Release
                {|
                    Judgement = Some judgement
                    Missed = is_miss
                    Delta = delta
                    Overhold = is_overhold
                    Dropped = is_dropped
                |}

        | HoldNoteBehaviour.Normal rules ->
            let head_judgement = head_judgements.[column]
            let judgement =
                if is_overhold && not is_dropped then
                    max head_judgement rules.JudgementIfOverheld
                elif is_miss || is_dropped then
                    max head_judgement rules.JudgementIfDropped
                else
                    head_judgement

            this.State.Add(judgement_to_points delta judgement, 1.0, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                this.State.BreakCombo true
            else
                this.State.IncrCombo()

            Release
                {|
                    Judgement = Some judgement
                    Missed = is_miss
                    Delta = delta
                    Overhold = is_overhold
                    Dropped = is_dropped
                |}

        | HoldNoteBehaviour.OnlyJudgeReleases judgement_if_dropped ->
            let judgement = 
                if is_overhold || is_dropped then judgement_if_dropped else ms_to_judgement delta
            this.State.Add(judgement_to_points delta judgement, 1.0, judgement)

            if ruleset.Judgements.[judgement].BreaksCombo then
                this.State.BreakCombo true
            else
                this.State.IncrCombo()

            Release
                {|
                    Judgement = Some judgement
                    Missed = is_miss
                    Delta = delta
                    Overhold = is_overhold
                    Dropped = is_dropped
                |}

    override this.HandleEvent ev =
        {
            Index = ev.Index
            Time = ev.Time
            Column = ev.Column
            Guts =
                match ev.Guts with
                | Hit_(delta, is_hold, is_missed) ->
                    if is_hold then
                        this.HandleHoldHead(ev.Column, delta, is_missed)
                    else 
                        this.HandleTap(delta, is_missed)

                | Release_(delta, is_miss, is_overhold, is_dropped, is_head_missed) ->
                    this.HandleHoldTail(ev.Column, delta, is_miss, is_overhold, is_dropped, is_head_missed)
        }

module ScoreProcessor =

    let create (ruleset: Ruleset) (keys: int) (replay: IReplayProvider) notes rate : ScoreProcessor =
        ScoreProcessor(ruleset, keys, replay, notes, rate)

    let run (ruleset: Ruleset) (keys: int) (replay: IReplayProvider) notes rate : ScoreProcessor =
        let scoring = ScoreProcessor(ruleset, keys, replay, notes, rate)
        scoring.Update Time.infinity
        scoring

    open Prelude.Charts.Processing

    let create_dummy (chart: ModdedChart) : ScoreProcessor =
        let ruleset = PremadeRulesets.SC.create 4
        create ruleset chart.Keys (StoredReplayProvider Array.empty) chart.Notes 1.0f
