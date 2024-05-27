namespace Prelude.Gameplay

open Prelude

type BPM = int

type PatternKey = BPM * float32 * float * Time

module PatternKey =
    
    let supersedes (bpm1, density1, accuracy1, time1) (bpm2, density2, accuracy2, time2) =
        bpm2 >= bpm1 && density2 >= density1 && accuracy2 >= accuracy1 && time2 >= time1

type PatternStats = Set<PatternKey>

module PatternStats =

    let create () = Set.empty

    let add_observation (bpm: float32, scaled_density: float32, accuracy: float, time: Time) (stats: PatternStats) : PatternStats =

        let bpm = floor (bpm / 5f) * 5f |> int 
        let scaled_density = scaled_density / 0.1f |> floor |> fun x -> x * 0.1f
        let accuracy = accuracy / 0.005 |> floor |> fun x -> x * 0.005

        let mutable is_superseded = false
        let mutable superseded_something = false

        let redundancies_removed = 
            Set.filter (fun key ->
                if key |> PatternKey.supersedes (bpm, scaled_density, accuracy, time) then
                    is_superseded <- true

                if (bpm, scaled_density, accuracy, time) |> PatternKey.supersedes key then 
                    superseded_something <- true
                    false
                else true
            ) stats
        if superseded_something || not is_superseded then
            redundancies_removed |> Set.add (bpm, scaled_density, accuracy, time)
        else redundancies_removed

        