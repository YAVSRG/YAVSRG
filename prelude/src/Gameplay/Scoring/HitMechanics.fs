namespace Prelude.Gameplay

open Prelude

[<Struct>]
type private HitDetection =
    | FOUND of index: int * delta: Time
    | BLOCKED
    | NOTFOUND

module private HitMechanics =

    let interlude (hit_data: InternalScoreData, miss_window: Time, cbrush_window: Time) (k: int, start_index: int, now: Time) : HitDetection =
        let mutable i = start_index
        let mutable closest_bad_note_delta = miss_window
        let mutable closest_note_index = -1
        let mutable closest_note_delta = miss_window
        let end_of_window = now + miss_window

        while i < hit_data.Length && InternalScore.offsetOf hit_data.[i] <= end_of_window do
            let struct (t, deltas, status) = hit_data.[i]
            let delta = now - t

            // Find unhit note that is closer than the current candidate
            if (status.[k] = HitStatus.HIT_REQUIRED || status.[k] = HitStatus.HIT_HOLD_REQUIRED) then
                if Time.abs closest_note_delta > Time.abs delta then
                    closest_note_index <- i
                    closest_note_delta <- delta

                // If new candidate is within cbrush window, stop looking resulting in earliest match being used
                // Otherwise keep looking for something closer and allow this note to be missed
                if Time.abs closest_note_delta < cbrush_window then
                    i <- hit_data.Length
            // Find hit note that got hit earlier than the cbrush window, and track how close it is
            elif
                status.[k] = HitStatus.HIT_ACCEPTED
                && deltas.[k] < -cbrush_window
            then
                if Time.abs closest_bad_note_delta > Time.abs delta then
                    closest_bad_note_delta <- delta

            i <- i + 1

        if closest_note_index >= 0 then
            if Time.abs closest_bad_note_delta < Time.abs closest_note_delta then
                BLOCKED
            else
                FOUND (closest_note_index, closest_note_delta)
        else
            NOTFOUND

    let etterna (hit_data: InternalScoreData, miss_window: Time) (k: int, start_index: int, now: Time) : HitDetection =
        let mutable i = start_index
        let mutable closest_note_index = -1
        let mutable closest_note_delta = miss_window
        let end_of_window = now + miss_window

        while i < hit_data.Length && InternalScore.offsetOf hit_data.[i] <= end_of_window do
            let struct (t, _, status) = hit_data.[i]
            let delta = now - t

            if (status.[k] = HitStatus.HIT_REQUIRED || status.[k] = HitStatus.HIT_HOLD_REQUIRED) then
                if Time.abs closest_note_delta > Time.abs delta then
                    closest_note_index <- i
                    closest_note_delta <- delta

            i <- i + 1

        if closest_note_index >= 0 then
            FOUND (closest_note_index, closest_note_delta)
        else
            NOTFOUND
    
    let osu (hit_data: InternalScoreData, miss_window: Time, window_100: Time) (k: int, start_index: int, now: Time) : HitDetection =
        let mutable i = start_index
        let mutable candidate_note_index = -1
        let mutable candidate_note_delta = miss_window
        let end_of_window = now + miss_window

        while i < hit_data.Length && InternalScore.offsetOf hit_data.[i] <= end_of_window do
            let struct (t, _, status) = hit_data.[i]
            let delta = now - t

            // Find earliest unhit note
            if (status.[k] = HitStatus.HIT_REQUIRED || status.[k] = HitStatus.HIT_HOLD_REQUIRED) && delta <= window_100 then
                candidate_note_index <- i
                candidate_note_delta <- delta
                i <- hit_data.Length

            i <- i + 1

        if candidate_note_index >= 0 then
            FOUND (candidate_note_index, candidate_note_delta)
        else
            NOTFOUND