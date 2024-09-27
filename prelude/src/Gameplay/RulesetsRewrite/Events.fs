namespace Prelude.Gameplay.RulesetsV2

open Prelude
open Prelude.Charts
open Prelude.Gameplay
open Prelude.Gameplay.RulesetsV2

type GameplayEvent<'Guts> =
    {
        Index: int
        Time: ChartTime
        Column: int
        Action: 'Guts
    }

type GameplayActionInternal =
    | HIT of delta: GameplayTime * missed: bool
    | HOLD of delta: GameplayTime * missed: bool
    | RELEASE of 
        release_delta: GameplayTime * 
        missed: bool * 
        overhold: bool * 
        dropped: bool * 
        head_delta: GameplayTime *
        missed_head: bool
    | DROP_HOLD
    | REGRAB_HOLD
    | GHOST_TAP

// todo: move to EventProcessing
type GameplayAction =
    | Hit of
        {|
            Judgement: (int * float) option
            Missed: bool
            Delta: GameplayTime
        |}
    | Hold of
        {|
            Judgement: (int * float) option
            Missed: bool
            Delta: GameplayTime
        |}
    | Release of
        {|
            Judgement: (int * float) option
            Missed: bool
            Delta: GameplayTime
            Overhold: bool
            Dropped: bool
        |}
    | DropHold
    | RegrabHold
    | GhostTap

[<Struct>]
type private HoldStateInternal =
    | H_NOTHING
    | H_HOLDING
    | H_DROPPED
    | H_REGRABBED
    | H_MISSED_HEAD_DROPPED
    | H_MISSED_HEAD_REGRABBED
    member this.IsDropped =
        match this with
        | H_DROPPED
        | H_REGRABBED
        | H_MISSED_HEAD_DROPPED
        | H_MISSED_HEAD_REGRABBED -> true
        | _ -> false
    member this.MissedHead =
        match this with
        | H_MISSED_HEAD_DROPPED
        | H_MISSED_HEAD_REGRABBED -> true
        | _ -> false
        

[<Struct>]
[<RequireQualifiedAccess>]
type HoldState =
    | Released
    | Holding
    | Dropped
    | MissedHead
    | InTheFuture
    member this.ShowInReceptor = this = Holding || this = Dropped || this = Released

/// This processor takes notes, replay data and a ruleset and outputs internal gameplay event markers for how the inputs map to certain actions like hitting a note
/// These internal actions are then processed further by rulesets to calculate accuracy, combo, score, etc

[<AbstractClass>]
type GameplayEventProcessor(ruleset: RulesetV2, keys: int, replay: IReplayProvider, notes: TimeArray<NoteRow>, rate: Rate) =
    inherit ReplayConsumer(keys, replay)

    let first_note = (TimeArray.first notes).Value.Time

    let early_note_window_raw, late_note_window_raw = 
        match ruleset.Judgements |> Seq.rev |> Seq.tryPick _.TimingWindows with
        | Some (early, late) -> early, late
        | None -> 0.0f<ms / rate>, 0.0f<ms / rate>
    
    let early_release_window_raw, late_release_window_raw = 
        match ruleset.HoldMechanics with
        | HoldMechanics.OnlyRequireHold window -> 
            -window, window
        | HoldMechanics.JudgeReleasesSeparately (windows, _) ->
            match windows |> Seq.rev |> Seq.tryPick id with
            | Some (early, late) -> early, late
            | None -> 0.0f<ms / rate>, 0.0f<ms / rate>
        | HoldMechanics.OnlyJudgeReleases _ -> 
            early_note_window_raw, late_note_window_raw
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.OsuMania w) ->
            -w.Window50, w.Window100
        | HoldMechanics.CombineHeadAndTail (HeadTailCombineRule.HeadJudgementOr (early, late, _, _)) ->
            early, late

    let early_note_window_scaled, late_note_window_scaled = early_note_window_raw * rate, late_note_window_raw * rate

    //let max_note_window_raw = max (abs early_note_window_raw) late_note_window_raw
    //let max_note_window_scaled = max_note_window_raw * rate

    let early_release_window_scaled, late_release_window_scaled = early_release_window_raw * rate, late_release_window_raw * rate

    let early_window_scaled = min early_release_window_scaled early_note_window_scaled
    let late_window_scaled = max late_release_window_scaled late_note_window_scaled

    //let max_release_window_raw = max (abs early_release_window_raw) late_release_window_raw
    //let max_release_window_scaled = max_release_window_raw * rate

    //let max_window_raw = max max_note_window_raw max_release_window_raw
    //let max_window_scaled = max max_note_window_scaled max_release_window_scaled


    let hold_states = Array.create keys (H_NOTHING, -1)

    let hit_data = HitFlagData.create_gameplay late_note_window_raw late_release_window_raw keys notes

    let hit_mechanics, PREVENT_BACKWARDS_NOTES =
        match ruleset.HitMechanics with
        | HitMechanics.Interlude cbrush_window ->
            HitMechanics.interlude (hit_data, early_note_window_scaled, late_note_window_scaled, cbrush_window, rate), true
        | HitMechanics.OsuMania ->
            HitMechanics.osu_mania (hit_data, early_note_window_scaled, late_note_window_scaled), true
        | HitMechanics.Etterna ->
            HitMechanics.etterna (hit_data, early_note_window_scaled, late_note_window_scaled), false

    let mutable expired_notes_index = 0
    let mutable note_seek_inputs = 0

    //member this.ScaledMissWindow = max_note_window_scaled
    member this.EnumerateRecentInputs() = replay.EnumerateRecentEvents()
    member this.Ruleset = ruleset

    member this.HoldState (index: int) (k: int) =
        let state, i = hold_states.[k]

        if i = index then
            match state with
            | H_NOTHING -> HoldState.Released
            | H_HOLDING -> HoldState.Holding
            | H_DROPPED
            | H_REGRABBED -> HoldState.Dropped
            | H_MISSED_HEAD_DROPPED
            | H_MISSED_HEAD_REGRABBED -> HoldState.MissedHead
        elif i > index then
            let { Data = struct (_, flags) } = hit_data.[index]

            if flags.[k] <> HitFlags.HIT_HOLD_REQUIRED then
                HoldState.Released
            else
                HoldState.MissedHead
        else
            HoldState.InTheFuture

    member this.IsNoteHit (index: int) (k: int) =
        let  { Data = struct (_, flags) } = hit_data.[index]
        flags.[k] = HitFlags.HIT_ACCEPTED

    member this.Finished = expired_notes_index = hit_data.Length

    /// Result of calling this: 
    ///  All unhit notes >[EARLY NOTE WINDOW] into the past are marked as misses as they can no longer be hit
    ///  All unreleased releases >[EARLY RELEASE WINDOW] into the past are marked as misses as they can no longer be released

    member private this.MissUnhitExpiredNotes(chart_time: ChartTime) =
        let now = first_note + chart_time
        let end_of_search = now + early_window_scaled

        while expired_notes_index < hit_data.Length
              && hit_data.[expired_notes_index].Time < end_of_search do
            let { Time = t; Data = struct(deltas, status) } = hit_data.[expired_notes_index]

            for k = 0 to (keys - 1) do

                if status.[k] = HitFlags.HIT_REQUIRED then
                    this.HandleEvent
                        {
                            Index = expired_notes_index
                            Time = t - first_note + late_note_window_scaled
                            Column = k
                            Action = HIT(deltas.[k], true)
                        }
                    status.[k] <- HitFlags.HIT_ACCEPTED

                elif status.[k] = HitFlags.HIT_HOLD_REQUIRED then
                    hold_states.[k] <- H_MISSED_HEAD_DROPPED, expired_notes_index

                    this.HandleEvent
                        {
                            Index = expired_notes_index
                            Time = t - first_note + late_note_window_scaled
                            Column = k
                            Action = HOLD(deltas.[k], true)
                        }
                    status.[k] <- HitFlags.HIT_ACCEPTED

                elif status.[k] = HitFlags.RELEASE_REQUIRED then
                    let overhold =
                        match hold_states.[k] with
                        | H_REGRABBED, i
                        | H_HOLDING, i
                        | H_MISSED_HEAD_REGRABBED, i when i <= expired_notes_index -> Bitmask.has_key k this.KeyState //todo: is guard needed
                        | _ -> false

                    let dropped =
                        match hold_states.[k] with
                        | H_DROPPED, _
                        | H_REGRABBED, _
                        | H_MISSED_HEAD_DROPPED, _
                        | H_MISSED_HEAD_REGRABBED, _ -> true
                        | _ -> false

                    let missed_head =
                        match hold_states.[k] with
                        | H_MISSED_HEAD_DROPPED, _
                        | H_MISSED_HEAD_REGRABBED, _ -> true
                        | _ -> false

                    let { Data = struct (head_deltas, _) } = hit_data.[snd hold_states.[k]]

                    this.HandleEvent
                        {
                            Index = expired_notes_index
                            Time = t - first_note + late_release_window_scaled
                            Column = k
                            Action = RELEASE(deltas.[k], true, overhold, dropped, head_deltas.[k], missed_head)
                        }
                    status.[k] <- HitFlags.RELEASE_ACCEPTED

                    match hold_states.[k] with
                    | _, i when i < expired_notes_index -> hold_states.[k] <- H_NOTHING, expired_notes_index
                    | _ -> ()

            expired_notes_index <- expired_notes_index + 1

    /// Some hit mechanics allow hitting a later note first, while an earlier note is still hittable
    /// To prevent another input then hitting the earlier note, all earlier notes are marked as missed
    /// See `PREVENT_BACKWARDS_NOTES`

    member private this.MissBackwardsNotes(k: int, before_index: int, chart_time: Time) =
        ()
        // ILL FIGURE IT OUT IN A BIT

    override this.HandleKeyDown(chart_time: ChartTime, k: int) =
        this.MissUnhitExpiredNotes chart_time
        let now = first_note + chart_time

        while note_seek_inputs < hit_data.Length
              && hit_data.[note_seek_inputs].Time < now - late_note_window_scaled do
            note_seek_inputs <- note_seek_inputs + 1

        match hit_mechanics (k, note_seek_inputs, now) with
        | BLOCKED -> ()
        | FOUND (index, delta) ->
            if PREVENT_BACKWARDS_NOTES then this.MissBackwardsNotes(k, index, chart_time)
            let { Data = struct (deltas, status) } = hit_data.[index]
            let is_hold_head = status.[k] <> HitFlags.HIT_REQUIRED
            status.[k] <- HitFlags.HIT_ACCEPTED
            deltas.[k] <- delta / rate

            this.HandleEvent
                {
                    Index = index
                    Time = chart_time
                    Column = k
                    Action = if is_hold_head then HOLD(deltas.[k], false) else HIT(deltas.[k], false)
                }
            // Begin tracking if it's a hold note
            if is_hold_head then
                hold_states.[k] <- H_HOLDING, index
        | NOTFOUND ->
            // If no note to hit, but a hold note head was missed, pressing key marks it dropped instead
            match hold_states.[k] with
            | H_MISSED_HEAD_DROPPED, i ->
                hold_states.[k] <- H_MISSED_HEAD_REGRABBED, i
                this.HandleEvent
                    {
                        Index = i
                        Time = chart_time
                        Column = k
                        Action = REGRAB_HOLD
                    }
            | H_DROPPED, i ->
                hold_states.[k] <- H_REGRABBED, i
                this.HandleEvent
                    {
                        Index = i
                        Time = chart_time
                        Column = k
                        Action = REGRAB_HOLD
                    }
            | H_NOTHING, i ->
                this.HandleEvent
                    {
                        Index = i
                        Time = chart_time
                        Column = k
                        Action = GHOST_TAP
                    }
            | _ -> ()

    override this.HandleKeyUp(chart_time: ChartTime, k: int) =
        this.MissUnhitExpiredNotes chart_time
        let now = first_note + chart_time

        match hold_states.[k] with
        | H_HOLDING, head_index
        | H_REGRABBED, head_index
        | H_MISSED_HEAD_REGRABBED, head_index ->

            let mutable i = head_index
            let mutable delta = late_note_window_scaled
            let mutable found = -1
            let target = now + late_note_window_scaled

            while i < hit_data.Length && hit_data.[i].Time <= target do
                let { Time = t; Data = struct (_, status) } = hit_data.[i]
                let d = now - t

                assert(status.[k] <> HitFlags.RELEASE_ACCEPTED) // we should never pass one tail and find another tail by mistake

                if status.[k] = HitFlags.RELEASE_REQUIRED then
                    // Get the first unreleased hold tail we see, after the head of the hold we're tracking
                    found <- i
                    delta <- d
                    i <- hit_data.Length

                i <- i + 1

            if found >= 0 then
                let { Data = struct (deltas, status) } = hit_data.[found]
                status.[k] <- HitFlags.RELEASE_ACCEPTED
                deltas.[k] <- delta / rate

                let { Data = struct (head_deltas, _) } = hit_data.[head_index]

                let hold_state = fst hold_states.[k]

                this.HandleEvent
                    {
                        Index = found
                        Time = chart_time
                        Column = k
                        Action =
                            RELEASE(
                                deltas.[k],
                                false,
                                false,
                                hold_state.IsDropped,
                                head_deltas.[k],
                                hold_state.MissedHead
                            )
                    }

                hold_states.[k] <- H_NOTHING, head_index
            else // If we released but too early (no sign of the tail within range) make the long note dropped
                
                match hold_states.[k] with
                | H_HOLDING, i
                | H_REGRABBED, i ->
                    hold_states.[k] <- H_DROPPED, i
                    this.HandleEvent
                        {
                            Index = i
                            Time = chart_time
                            Column = k
                            Action = DROP_HOLD
                        }
                | H_MISSED_HEAD_REGRABBED, i ->
                    hold_states.[k] <- H_MISSED_HEAD_DROPPED, i
                    this.HandleEvent
                        {
                            Index = i
                            Time = chart_time
                            Column = k
                            Action = DROP_HOLD
                        }
                | _ -> ()
        | _ -> ()

    /// Result of calling this with a particular `chart_time`:
    /// - All replay inputs up to `chart_time` have been processed into hits by finding which notes/releases they correspond to with what timing
    /// - Notes where the time has passed to hit them, with no input mapping to them, have been processed as misses
    
    /// Expected to be called with monotonically increasing values of `chart_time` during gameplay for live scoring
    /// For processing an entire score instantly pass in Time.infinity or any time higher than the duration of the chart + latest hit window

    member this.Update(chart_time: ChartTime) =
        this.PollReplay chart_time // Process all key presses and releases up until now
        this.MissUnhitExpiredNotes chart_time // Then process any missed notes up until now if needed

    abstract member HandleEvent: GameplayEvent<GameplayActionInternal> -> unit