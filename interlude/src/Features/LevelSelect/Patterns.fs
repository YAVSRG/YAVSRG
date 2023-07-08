namespace Interlude.Features.LevelSelect

open Prelude.Charts.Tools.Patterns
open Interlude.Features
open Interlude.UI

module Patterns =

    let mutable report = []

    let update_report() =
        let duration = Gameplay.Chart.cacheInfo.Value.Length / (Gameplay.rate.Value * 1.0f<rate>)
        let data = 
            Patterns.analyse Gameplay.rate.Value Gameplay.Chart.current.Value
            |> Patterns.pattern_locations
            |> Patterns.pattern_breakdown

        let importance (p, bpm) =
            match p with
            | Stream s -> float32 (bpm * bpm) * 0.5f
            | Jack s -> float32 (bpm * bpm)

        report <-
        seq {
            for (p, bpm) in data.Keys |> Seq.sortByDescending(fun k -> data.[k].TotalTime * importance k) do
                let d = data.[(p, bpm)]
                let percent = d.TotalTime * 100.0f / duration

                let category =
                    if d.Marathons.Count > 0 then "stamina"
                    elif d.Sprints.Count > 1 then "sprints"
                    elif d.Sprints.Count = 1 then "sprint"
                    elif d.Runs.Count > 1 then "runs"
                    elif d.Runs.Count = 1 then "run"
                    elif d.Bursts.Count > 1 then "bursts"
                    else "burst"

                let density = d.DensityTime / d.TotalTime
                let max_density = float32 bpm / 15f
                let name, density_ratio =
                    match p with
                    | Stream s -> s, density * 200.0f / max_density
                    | Jack s -> s, (density * 200.0f / max_density) - 50.0f
                yield (name, bpm)
        } |> List.ofSeq

    let display() =
        let mutable c = Callout.Normal.Title("What's in this chart?")
        for (pattern, bpm) in List.truncate 5 report do
            c <- c.Body(sprintf "%i BPM %s" bpm pattern)
        c.Body("^ These are the top matches. Pattern analysis is a WIP")