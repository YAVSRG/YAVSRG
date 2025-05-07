namespace Prelude.Gameplay.Scoring

open Prelude
open Prelude.Charts
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Rulesets

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

type GameplayEventInternal =
    {
        Index: int
        Time: ChartTime
        Column: int
        Action: GameplayActionInternal
    }

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
type GameplayEventProcessor(ruleset: Ruleset, keys: int, replay: IReplay, notes: TimeArray<NoteRow>, rate: Rate) =
    inherit ReplayConsumer(keys, replay)

    let first_note = (TimeArray.first notes).Value.Time

    let early_note_window_raw, late_note_window_raw = ruleset.NoteWindows
    let early_release_window_raw, late_release_window_raw = ruleset.ReleaseWindows

    let early_note_window_scaled, late_note_window_scaled = early_note_window_raw * rate, late_note_window_raw * rate
    let early_release_window_scaled, late_release_window_scaled = early_release_window_raw * rate, late_release_window_raw * rate

    let early_window_scaled = min early_release_window_scaled early_note_window_scaled
    let late_window_scaled = max late_release_window_scaled late_note_window_scaled

    let hold_states = Array.create keys (H_NOTHING, -1)

    let hit_data = HitFlagData.create_gameplay late_note_window_raw late_release_window_raw keys notes

    let hit_mechanics =
        match ruleset.HitMechanics.NotePriority with
        | NotePriority.Interlude cbrush_window ->
            HitMechanics.interlude (hit_data, early_note_window_scaled, late_note_window_scaled, cbrush_window, rate)
        | NotePriority.OsuMania ->
            HitMechanics.osu_mania (hit_data, early_note_window_scaled, late_note_window_scaled)
        | NotePriority.Etterna ->
            HitMechanics.etterna (hit_data, early_note_window_scaled, late_note_window_scaled)

    let mutable expired_notes_index = 0

    member private this.KillExistingHold (chart_time: ChartTime) (k: int) =
        match hold_states.[k] with
        | H_NOTHING, _ -> ()
        | hold_state, head_index ->
            let mutable tail_search_index = head_index

            while tail_search_index < hit_data.Length do
                let { Data = struct (_, status) } = hit_data.[tail_search_index]

                // Only possible if an LN has been modified to be already hit via `IgnoreNotesBefore`, for practice mode
                if status.[k] = HitFlags.RELEASE_ACCEPTED then
                    tail_search_index <- hit_data.Length

                elif status.[k] = HitFlags.RELEASE_REQUIRED then

                    status.[k] <- HitFlags.RELEASE_ACCEPTED
                    hold_states.[k] <- H_NOTHING, head_index

                    let { Data = struct (head_deltas, _) } = hit_data.[head_index]

                    this.HandleEvent
                        {
                            Index = tail_search_index
                            Time = chart_time
                            Column = k
                            Action =
                                RELEASE(
                                    late_release_window_raw,
                                    true,
                                    false,
                                    true,
                                    head_deltas.[k],
                                    hold_state.MissedHead
                                )
                        }

                    tail_search_index <- hit_data.Length

                tail_search_index <- tail_search_index + 1

    member this.EnumerateRecentFrames() = replay.EnumerateRecentFrames()
    member this.Ruleset = ruleset

    member this.HoldState (index: int) (k: int) : HoldState =
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

    member this.IsNoteHit (index: int) (k: int) : bool =
        let  { Data = struct (_, flags) } = hit_data.[index]
        flags.[k] = HitFlags.HIT_ACCEPTED

    member this.Finished : bool = expired_notes_index = hit_data.Length

    /// Result of calling this:
    ///  notes.[expired_notes_index - 1].Time < now - LATE WINDOW
    ///  notes.[expired_notes_index].Time >= now - LATE WINDOW
    ///  All notes on rows before `expired_notes_index` now have an action assigned - if there wasn't one before, it is now marked as missed

    member private this.MissUnhitExpiredNotes(chart_time: ChartTime) =
        let now = first_note + chart_time
        let end_of_search = now - late_window_scaled

        while expired_notes_index < hit_data.Length
              && hit_data.[expired_notes_index].Time < end_of_search do
            let { Time = t; Data = struct(deltas, status) } = hit_data.[expired_notes_index]

            for k = 0 to (keys - 1) do

                if status.[k] = HitFlags.HIT_REQUIRED then
                    this.HandleEvent
                        {
                            Index = expired_notes_index
                            Time = t - first_note + late_window_scaled
                            Column = k
                            Action = HIT(deltas.[k], true)
                        }
                    status.[k] <- HitFlags.HIT_ACCEPTED

                elif status.[k] = HitFlags.HIT_HOLD_REQUIRED then
                    hold_states.[k] <- H_MISSED_HEAD_DROPPED, expired_notes_index

                    this.HandleEvent
                        {
                            Index = expired_notes_index
                            Time = t - first_note + late_window_scaled
                            Column = k
                            Action = HOLD(deltas.[k], true)
                        }
                    status.[k] <- HitFlags.HIT_ACCEPTED

                elif status.[k] = HitFlags.RELEASE_REQUIRED then
                    let overhold =
                        match fst hold_states.[k] with
                        | H_REGRABBED
                        | H_HOLDING
                        | H_MISSED_HEAD_REGRABBED -> Bitmask.has_key k this.KeyState
                        | _ -> false

                    let dropped = (fst hold_states.[k]).IsDropped

                    let missed_head = (fst hold_states.[k]).MissedHead

                    let { Data = struct (head_deltas, _) } = hit_data.[snd hold_states.[k]]

                    this.HandleEvent
                        {
                            Index = expired_notes_index
                            Time = t - first_note + late_window_scaled
                            Column = k
                            Action = RELEASE(deltas.[k], true, overhold, dropped, head_deltas.[k], missed_head)
                        }
                    status.[k] <- HitFlags.RELEASE_ACCEPTED

                    match hold_states.[k] with
                    | _, i when i < expired_notes_index -> hold_states.[k] <- H_NOTHING, expired_notes_index
                    | _ -> ()

            expired_notes_index <- expired_notes_index + 1

    override this.HandleKeyDown(chart_time: ChartTime, k: int) =
        this.MissUnhitExpiredNotes chart_time
        let now = first_note + chart_time

        let mutable start_of_search_index = expired_notes_index

        while start_of_search_index < hit_data.Length
              && hit_data.[start_of_search_index].Time < now - late_note_window_scaled do
            start_of_search_index <- start_of_search_index + 1

        match hit_mechanics (k, start_of_search_index, now) with
        | BLOCKED -> ()
        | FOUND (index, delta) ->
            this.KillExistingHold chart_time k
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
            | H_NOTHING, _ when chart_time > 0.0f<ms> ->
                this.HandleEvent
                    {
                        Index = expired_notes_index
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

            let mutable tail_search_index = head_index
            let mutable delta = 0.0f<ms>
            let mutable found = -1

            while tail_search_index < hit_data.Length && hit_data.[tail_search_index].Time <= now - early_window_scaled do
                let { Time = t; Data = struct (_, status) } = hit_data.[tail_search_index]
                let d = now - t

                // Only possible if an LN has been modified to be already hit via `IgnoreNotesBefore`, for practice mode
                if status.[k] = HitFlags.RELEASE_ACCEPTED then
                    tail_search_index <- hit_data.Length

                elif status.[k] = HitFlags.RELEASE_REQUIRED then
                    found <- tail_search_index
                    delta <- d
                    tail_search_index <- hit_data.Length

                tail_search_index <- tail_search_index + 1

            if found >= 0 && delta >= early_release_window_scaled then
                let { Data = struct (deltas, status) } = hit_data.[found]
                status.[k] <- HitFlags.RELEASE_ACCEPTED

                let overhold =
                    if delta > late_release_window_scaled then
                        true
                    else
                        deltas.[k] <- delta / rate
                        false

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
                                overhold,
                                overhold,
                                hold_state.IsDropped,
                                head_deltas.[k],
                                hold_state.MissedHead
                            )
                    }

                hold_states.[k] <- H_NOTHING, head_index
            else
                // Early release! Mark hold as dropped
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
    /// For processing an entire score instantly pass in Time.infinity or any time higher than the duration of the chart + late window

    member this.Update(chart_time: ChartTime) =
        this.PollReplay chart_time // Process all key presses and releases up until now
        this.MissUnhitExpiredNotes chart_time // Then process any missed notes up until now if needed

    abstract member HandleEvent: GameplayEventInternal -> unit

    /// Used by practice mode in the client to enter gameplay midway through a song
    /// Prevents accumulating misses for every single note before this point or column locking on the way in
    member this.IgnoreNotesBefore(time: Time) =
        let mutable i = 0

        let mutable hold_tails_remaining = 0us

        while i < hit_data.Length && hit_data.[i].Time < time do
            let { Data = struct (deltas, flags) } = hit_data.[i]

            for k = 0 to keys - 1 do
                if flags.[k] = HitFlags.HIT_REQUIRED then
                    flags.[k] <- HitFlags.HIT_ACCEPTED
                elif flags.[k] = HitFlags.HIT_HOLD_REQUIRED then
                    flags.[k] <- HitFlags.HIT_ACCEPTED
                    hold_tails_remaining <- Bitmask.set_key k hold_tails_remaining
                elif flags.[k] = HitFlags.RELEASE_REQUIRED then
                    flags.[k] <- HitFlags.RELEASE_ACCEPTED
                    hold_tails_remaining <- Bitmask.unset_key k hold_tails_remaining
                deltas.[k] <- -infinityf * 1.0f<ms / rate>

            i <- i + 1

        while hold_tails_remaining > 0us do
            let { Data = struct (deltas, flags) } = hit_data.[i]

            for k = 0 to keys - 1 do
                if flags.[k] = HitFlags.RELEASE_REQUIRED then
                    flags.[k] <- HitFlags.RELEASE_ACCEPTED
                    hold_tails_remaining <- Bitmask.unset_key k hold_tails_remaining
                    deltas.[k] <- -infinityf * 1.0f<ms / rate>

            i <- i + 1