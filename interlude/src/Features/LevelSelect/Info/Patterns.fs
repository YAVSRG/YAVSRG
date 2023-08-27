namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Charts.Tools.Patterns
open Interlude.Features

module Patterns =

    let mutable report = []

    let update_report() =
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
            let items =
                data.Keys 
                |> Seq.map ( fun (p, bpm) -> (p, bpm, data.[(p, bpm)].TotalTime * importance (p, bpm) / 1_000_000.0f) )
                |> Seq.sortByDescending (fun (_, _, x) -> x )
            for (p, bpm, points) in items do
                let d = data.[(p, bpm)]
                yield (p, bpm, points)
        } |> List.ofSeq

    let display =
        { new StaticWidget(NodeType.None, Position = Position.TrimBottom(120.0f)) with 
            override this.Draw() =
                Text.drawFillB(Style.font, "What's in this chart?", this.Bounds.SliceTop(70.0f).Shrink(10.0f), Colors.text, Alignment.CENTER)
                let mutable b = this.Bounds.SliceTop(40.0f).Shrink(10.0f, 0.0f).Translate(0.0f, 70.0f)
                for (pattern, bpm, points) in List.truncate 10 report do
                    Text.drawFillB(Style.font, sprintf "%i BPM %A [%f]" bpm pattern points, b, Colors.text_subheading, Alignment.CENTER)
                    b <- b.Translate(0.0f, 45.0f)
        }