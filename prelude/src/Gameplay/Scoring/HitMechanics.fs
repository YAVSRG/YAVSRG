namespace Prelude.Gameplay.Scoring

open Prelude
open Prelude.Charts

// Array of data for marking notes as hit/unhit for hit mechanics

type HitFlags =
    | NOTHING = 0

    | HIT_REQUIRED = 1
    | HIT_HOLD_REQUIRED = 2
    | HIT_ACCEPTED = 3

    | RELEASE_REQUIRED = 4
    | RELEASE_ACCEPTED = 5

type HitFlagData = TimeArray<struct(GameplayTime array * HitFlags array)>

module HitFlagData =

    let create_gameplay (note_window: GameplayTime) (release_window: GameplayTime) (keys: int) (notes: TimeArray<NoteRow>) : HitFlagData =
        notes
        |> TimeArray.map (fun nr ->
            let deltas = Array.create keys note_window
            let statuses = Array.create keys HitFlags.NOTHING

            for k = 0 to (keys - 1) do
                if nr.[k] = NoteType.NORMAL then
                    statuses.[k] <- HitFlags.HIT_REQUIRED
                elif nr.[k] = NoteType.HOLDHEAD then
                    statuses.[k] <- HitFlags.HIT_HOLD_REQUIRED
                elif nr.[k] = NoteType.HOLDTAIL then
                    statuses.[k] <- HitFlags.RELEASE_REQUIRED
                    deltas.[k] <- release_window

            struct (deltas, statuses)
        )

// Detect hits

[<Struct>]
type private HitDetection =
    | FOUND of index: int * delta: Time
    | BLOCKED
    | NOTFOUND

module private HitMechanics =

    let interlude (hit_data: HitFlagData, early_window: Time, late_window: Time, cbrush_window_raw: GameplayTime, rate: Rate) (k: int, start_index: int, now: Time) : HitDetection =
        let mutable i = start_index
        let mutable closest_bad_note_delta = late_window
        let mutable closest_note_index = -1
        let mutable closest_note_delta = late_window
        let start_of_window = now - late_window
        let end_of_window = now - early_window
        let cbrush_window = cbrush_window_raw * rate

        assert(cbrush_window >= 0.0f<ms>)
        assert(early_window <= 0.0f<ms>)
        //assert(i >= hit_data.Length || hit_data.[i].Time >= start_of_window)
        //assert(i = 0 || hit_data.[i - 1].Time < start_of_window)

        while i < hit_data.Length && hit_data.[i].Time <= end_of_window do
            let delta = now - hit_data.[i].Time
            let struct (deltas, status) = hit_data.[i].Data

            // Find unhit note that is closer than the current candidate
            if (status.[k] = HitFlags.HIT_REQUIRED || status.[k] = HitFlags.HIT_HOLD_REQUIRED) then
                if closest_note_index < 0 || abs closest_note_delta > abs delta then
                    closest_note_index <- i
                    closest_note_delta <- delta

                // If new candidate is within cbrush window, stop looking resulting in earliest match being used
                // Otherwise keep looking for something closer and allow this note to be missed
                if abs closest_note_delta < cbrush_window then
                    i <- hit_data.Length
            // Find hit note that got hit earlier than the cbrush window, and track how close it is
            elif
                status.[k] = HitFlags.HIT_ACCEPTED
                && deltas.[k] <= -cbrush_window_raw
            then
                if abs closest_bad_note_delta > abs delta then
                    closest_bad_note_delta <- delta

            i <- i + 1

        if closest_note_index >= 0 then
            if abs closest_bad_note_delta < abs closest_note_delta then
                BLOCKED
            else
                FOUND (closest_note_index, closest_note_delta)
        else
            NOTFOUND

    let etterna (hit_data: HitFlagData, early_window: Time, late_window: Time) (k: int, start_index: int, now: Time) : HitDetection =
        let mutable i = start_index
        let mutable closest_note_index = -1
        let mutable closest_note_delta = 0.0f<ms>
        let start_of_window = now - late_window
        let end_of_window = now - early_window

        assert(early_window <= 0.0f<ms>)
        //assert(i >= hit_data.Length || hit_data.[i].Time >= start_of_window)
        //assert(i = 0 || hit_data.[i - 1].Time < start_of_window)

        while i < hit_data.Length && hit_data.[i].Time <= end_of_window do
            let delta = now - hit_data.[i].Time
            let struct (_, status) = hit_data.[i].Data

            // Find closest unhit note
            if (status.[k] = HitFlags.HIT_REQUIRED || status.[k] = HitFlags.HIT_HOLD_REQUIRED) then
                if closest_note_index < 0 || abs closest_note_delta > abs delta then
                    closest_note_index <- i
                    closest_note_delta <- delta

            i <- i + 1

        if closest_note_index >= 0 then
            FOUND (closest_note_index, closest_note_delta)
        else
            NOTFOUND

    let osu_mania (hit_data: HitFlagData, early_window: Time, late_window: Time) (k: int, start_index: int, now: Time) : HitDetection =
        let mutable i = start_index
        let mutable candidate_note_index = -1
        let mutable candidate_note_delta = 0.0f<ms>
        let mutable blocked = false
        let start_of_window = now - late_window
        let end_of_window = now - early_window

        assert(early_window <= 0.0f<ms>)
        //assert(i >= hit_data.Length || hit_data.[i].Time >= start_of_window)
        //assert(i = 0 || hit_data.[i - 1].Time < start_of_window)

        while i < hit_data.Length && hit_data.[i].Time <= end_of_window do
            let delta = now - hit_data.[i].Time
            let struct (_, status) = hit_data.[i].Data

            if (status.[k] = HitFlags.HIT_ACCEPTED && delta < 0.0f<ms>) then
                blocked <- true
                i <- hit_data.Length

            // Find earliest unhit note
            if (status.[k] = HitFlags.HIT_REQUIRED || status.[k] = HitFlags.HIT_HOLD_REQUIRED) then
                candidate_note_index <- i
                candidate_note_delta <- delta
                i <- hit_data.Length

            i <- i + 1

        if blocked then
            BLOCKED
        elif candidate_note_index >= 0 then
            FOUND (candidate_note_index, candidate_note_delta)
        else
            NOTFOUND