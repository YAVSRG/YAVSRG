namespace Prelude.Gameplay

open Prelude

type BPM = int

type PatternKey = BPM * float32 * float

module PatternKey =
    
    let supersedes (bpm1, density1, accuracy1) (bpm2, density2, accuracy2) =
        bpm2 >= bpm1 && density2 >= density1 && accuracy2 >= accuracy1

type PatternStats = Map<PatternKey, Time>

module PatternStats =

    let create () = Map.empty

    let add_observation (bpm: float32, scaled_density: float32, accuracy: float) (time: Time) (stats: PatternStats) : PatternStats =

        let bpm = floor (bpm / 5f) * 5f |> int 
        let scaled_density = scaled_density / 0.1f |> floor |> fun x -> x * 0.1f

        let mutable is_superseded = false
        let mutable superseded_something = false

        let redundancies_removed = 
            Map.filter (fun key value ->
                if key |> PatternKey.supersedes (bpm, scaled_density, accuracy) && value >= time then
                    is_superseded <- true

                if (bpm, scaled_density, accuracy) |> PatternKey.supersedes key && value < time then 
                    superseded_something <- true
                    false
                else true
            ) stats
        if superseded_something || not is_superseded then
            redundancies_removed |> Map.add (bpm, scaled_density, accuracy) time
        else redundancies_removed

        