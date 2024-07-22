namespace Prelude.Gameplay

open Prelude

type BPM = int

type PatternKey = BPM * float * Time

module PatternKey =
    
    let supersedes (bpm1, accuracy1, time1) (bpm2, accuracy2, time2) =
        bpm2 >= bpm1 && accuracy2 >= accuracy1 && time2 >= time1

    let max (bpm1, accuracy1, time1) (bpm2, accuracy2, time2) =
        max bpm1 bpm2, max accuracy1 accuracy2, max time1 time2

type PatternStats = Set<PatternKey>

module PatternStats =

    let create () = Set.empty

    let add_observation (bpm: float32, accuracy: float, time: Time) (stats: PatternStats) : PatternStats =

        let incoming_key = 
            floor (bpm / 5f) * 5f |> int,
            (accuracy / 0.02 |> floor |> fun x -> x * 0.02),
            time

        let mutable is_superseded = false

        let redundancies_removed = 
            Set.map (fun key ->
                if key |> PatternKey.supersedes incoming_key then
                    is_superseded <- true
                    key

                elif incoming_key |> PatternKey.supersedes key then 
                    incoming_key

                else key
            ) stats
        if not is_superseded then
            redundancies_removed |> Set.add incoming_key
        else redundancies_removed