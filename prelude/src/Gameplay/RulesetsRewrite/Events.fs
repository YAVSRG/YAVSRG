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
    | HIT of delta: Time * missed: bool
    | HOLD of delta: Time * missed: bool
    | RELEASE of 
        release_delta: Time * 
        missed: bool * 
        overhold: bool * 
        dropped: bool * 
        head_delta: Time *
        missed_head: bool
    | BREAK_HOLD
    | REGRAB_HOLD
    | GHOST_TAP

type GameplayAction =
    | Hit of
        {|
            Judgement: (int * float) option
            Missed: bool
            Delta: Time
        |}
    | Hold of
        {|
            Judgement: (int * float) option
            Missed: bool
            Delta: Time
        |}
    | Release of
        {|
            Judgement: (int * float) option
            Missed: bool
            Delta: Time
            Overhold: bool
            Dropped: bool
        |}
    | BreakHold
    | RegrabHold
    | GhostTap

[<Struct>]
type private HoldStateInternal =
    | H_NOTHING
    | H_HOLDING
    | H_DROPPED
    | H_MISSED_HEAD_THEN_HELD
    | H_MISSED_HEAD

[<Struct>]
[<RequireQualifiedAccess>]
type HoldState =
    | Released
    | Holding
    | Dropped
    | MissedHead
    | InTheFuture
    member this.ShowInReceptor = this = Holding || this = Dropped || this = Released

[<AbstractClass>]
type GameplayEventProcessor(ruleset: RulesetV2, keys: int, replay: IReplayProvider, notes: TimeArray<NoteRow>, rate: float32) =
    inherit ReplayConsumer(keys, replay)

    let first_note = (TimeArray.first notes).Value.Time
    let last_note = (TimeArray.last notes).Value.Time
    let duration = last_note - first_note
    let early_note_window_raw, late_note_window_raw = 
        match ruleset.Judgements |> Seq.rev |> Seq.tryPick _.TimingWindows with
        | Some (early, late) -> early, late
        | None -> 0.0f<ms>, 0.0f<ms>

    let early_note_window_scaled, late_note_window_scaled = early_note_window_raw * rate, late_note_window_raw * rate

    let max_note_window_raw = max (abs early_note_window_raw) (abs late_note_window_raw)
    let max_note_window_scaled = max_note_window_raw * rate
    // ln windows

    let mutable note_seek_misses = 0
    let mutable note_seek_inputs = 0

    let hold_states = Array.create keys (H_NOTHING, -1)

    // todo: pass in ln windows too and set those deltas correctly
    let hit_data = HitFlagData.create_gameplay max_note_window_raw keys notes

    let hit_mechanics, PREVENT_BACKWARDS_NOTES =
        match ruleset.HitMechanics with
        | HitMechanics.Interlude cbrush_window ->
            HitMechanics.interlude (hit_data, early_note_window_scaled, late_note_window_scaled, cbrush_window), true
        | HitMechanics.OsuMania ->
            HitMechanics.osu_mania (hit_data, early_note_window_scaled, late_note_window_scaled), true
        | HitMechanics.Etterna ->
            HitMechanics.etterna (hit_data, early_note_window_scaled, late_note_window_scaled), false

    member this.ScaledMissWindow = max_note_window_scaled
    member this.EnumerateRecentInputs() = replay.EnumerateRecentEvents()
    member this.Ruleset = ruleset

    member this.HoldState (index: int) (k: int) =
        let state, i = hold_states.[k]

        if i = index then
            match state with
            | H_NOTHING -> HoldState.Released
            | H_HOLDING -> HoldState.Holding
            | H_DROPPED -> HoldState.Dropped
            | H_MISSED_HEAD
            | H_MISSED_HEAD_THEN_HELD -> HoldState.MissedHead
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

    member this.Finished = note_seek_misses = hit_data.Length

    member private this.HandleMissedNotes(chart_time: ChartTime) =
        let now = first_note + chart_time
        let target = now - early_note_window_scaled

        while note_seek_misses < hit_data.Length
              && hit_data.[note_seek_misses].Time <= target do
            let { Time = t; Data = struct(deltas, status) } = hit_data.[note_seek_misses]

            for k = 0 to (keys - 1) do

                if status.[k] = HitFlags.HIT_REQUIRED then
                    this.HandleEvent
                        {
                            Index = note_seek_misses
                            Time = t - first_note + max_note_window_scaled
                            Column = k
                            Action = HIT(deltas.[k], true)
                        }
                    status.[k] <- HitFlags.HIT_ACCEPTED

                elif status.[k] = HitFlags.HIT_HOLD_REQUIRED then
                    hold_states.[k] <- H_MISSED_HEAD, note_seek_misses

                    this.HandleEvent
                        {
                            Index = note_seek_misses
                            Time = t - first_note + max_note_window_scaled
                            Column = k
                            Action = HOLD(deltas.[k], true)
                        }
                    status.[k] <- HitFlags.HIT_ACCEPTED

                elif status.[k] = HitFlags.RELEASE_REQUIRED then
                    let overhold =
                        match hold_states.[k] with
                        | H_DROPPED, i
                        | H_HOLDING, i when i <= note_seek_misses -> Bitmask.has_key k this.KeyState
                        | _ -> false

                    let dropped =
                        match hold_states.[k] with
                        | H_DROPPED, _
                        | H_MISSED_HEAD_THEN_HELD, _
                        | H_MISSED_HEAD, _ -> true
                        | _ -> false

                    let missed_head =
                        match hold_states.[k] with
                        | H_MISSED_HEAD_THEN_HELD, _ -> true
                        | _ -> false

                    let { Data = struct (head_deltas, _) } = hit_data.[snd hold_states.[k]]

                    this.HandleEvent
                        {
                            Index = note_seek_misses
                            Time = t - first_note + max_note_window_scaled//max_release_window_scaled
                            Column = k
                            Action = RELEASE(deltas.[k], true, overhold, dropped, head_deltas.[k], missed_head)
                        }
                    status.[k] <- HitFlags.RELEASE_ACCEPTED

                    match hold_states.[k] with
                    | _, i when i < note_seek_misses -> hold_states.[k] <- H_NOTHING, note_seek_misses
                    | _ -> ()

            note_seek_misses <- note_seek_misses + 1

    member private this.MissPreviousNotes(k: int, before_index: int, chart_time: Time) =
        let mutable i = note_seek_misses
        while i < before_index do
            let { Time = t; Data = struct(deltas, status) } = hit_data.[i]

            if status.[k] = HitFlags.HIT_REQUIRED then
                this.HandleEvent
                    {
                        Index = i
                        Time = chart_time
                        Column = k
                        Action = HIT(deltas.[k], true)
                    }
                status.[k] <- HitFlags.HIT_ACCEPTED

            elif status.[k] = HitFlags.HIT_HOLD_REQUIRED then
                this.HandleEvent
                    {
                        Index = i
                        Time = chart_time
                        Column = k
                        Action = HOLD(deltas.[k], true)
                    }
                status.[k] <- HitFlags.HIT_ACCEPTED

            elif status.[k] = HitFlags.RELEASE_REQUIRED then
                this.HandleEvent
                    {
                        Index = i
                        Time = chart_time
                        Column = k
                        Action = RELEASE(deltas.[k], true, false, true, late_note_window_raw, true)
                    }
                status.[k] <- HitFlags.RELEASE_ACCEPTED

            i <- i + 1

    override this.HandleKeyDown(chart_time: ChartTime, k: int) =
        this.HandleMissedNotes chart_time
        let now = first_note + chart_time

        while note_seek_inputs < hit_data.Length
              && hit_data.[note_seek_inputs].Time < now - late_note_window_scaled do
            note_seek_inputs <- note_seek_inputs + 1

        match hit_mechanics (k, note_seek_inputs, now) with
        | BLOCKED -> ()
        | FOUND (index, delta) ->
            let { Time = t; Data = struct (deltas, status) } = hit_data.[index]
            if PREVENT_BACKWARDS_NOTES then this.MissPreviousNotes(k, index, chart_time)
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
            hold_states.[k] <-
                match hold_states.[k] with
                | H_MISSED_HEAD, i -> H_MISSED_HEAD_THEN_HELD, i
                | x -> x

    override this.HandleKeyUp(chart_time: ChartTime, k: int) =
        this.HandleMissedNotes chart_time
        let now = first_note + chart_time

        match hold_states.[k] with
        | H_HOLDING, head_index
        | H_DROPPED, head_index
        | H_MISSED_HEAD_THEN_HELD, head_index ->

            let mutable i = head_index
            let mutable delta = late_note_window_scaled
            let mutable found = -1
            let target = now + late_note_window_scaled

            while i < hit_data.Length && hit_data.[i].Time <= target do
                let { Time = t; Data = struct (deltas, status) } = hit_data.[i]
                let d = now - t

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
                                fst hold_states.[k] = H_DROPPED || fst hold_states.[k] = H_MISSED_HEAD_THEN_HELD,
                                head_deltas.[k],
                                fst hold_states.[k] = H_MISSED_HEAD_THEN_HELD
                            )
                    }

                hold_states.[k] <- H_NOTHING, head_index
            else // If we released but too early (no sign of the tail within range) make the long note dropped
                hold_states.[k] <-
                    match hold_states.[k] with
                    | H_HOLDING, i -> 
                        this.HandleEvent
                            {
                                Index = i
                                Time = chart_time
                                Column = k
                                Action = BREAK_HOLD
                            }
                        H_DROPPED, i
                    | x -> x
        | H_MISSED_HEAD, _
        | H_NOTHING, _ -> ()

    abstract member HandleEvent: GameplayEvent<GameplayActionInternal> -> unit
    
    member this.Update(chart_time: ChartTime) =
        this.PollReplay chart_time // calls HandleKeyDown and HandleKeyUp appropriately
        this.HandleMissedNotes chart_time