namespace Interlude.Features.Stats

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Gameplay
open Prelude.Data
open Interlude.Content

type SkillsetGraph(target: PatternSkillBreakdown) =
    inherit StaticWidget(NodeType.None)

    let survival = target.Survival |> Array.ofList |> Array.rev

    let min_bpm = (survival |> Seq.map _.BPM |> Seq.min |> float32) - 5.0f
    let max_bpm = (survival |> Seq.map _.BPM |> Seq.max |> float32)
    let max_duration = survival |> Seq.map _.Duration |> Seq.max

    let normal = target.Normal |> Array.ofList |> Array.rev
    let control = target.Control |> Array.ofList |> Array.rev
    let accuracy = target.Accuracy |> Array.ofList |> Array.rev

    override this.Draw() =
        let x bpm = this.Bounds.Left + (float32 bpm - min_bpm) / (max_bpm - min_bpm) * this.Bounds.Width
        let y d = this.Bounds.Bottom - d / max_duration * this.Bounds.Height
        let mutable last = min_bpm
        for point in survival do
            Draw.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), this.Bounds.Bottom)) Colors.blue
            last <- float32 point.BPM

        let mutable last = min_bpm
        for point in normal do
            Draw.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), this.Bounds.Bottom)) Colors.green
            last <- float32 point.BPM
        
        let mutable last = min_bpm
        for point in control do
            Draw.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), this.Bounds.Bottom)) Colors.yellow_accent
            last <- float32 point.BPM
        
        let mutable last = min_bpm
        for point in accuracy do
            Draw.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), this.Bounds.Bottom)) Colors.white
            last <- float32 point.BPM

type Skills() =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        //Skillsets.calculate Content.Scores Content.Library
        this 
        |+ SkillsetGraph(Skillsets.skills.Stream, Position = { Position.Margin(20.0f) with Bottom = 0.33f %- 10.0f })
        |+ SkillsetGraph(Skillsets.skills.Chordstream, Position = { Position.Margin(20.0f) with Top = 0.33f %+ 10.0f; Bottom = 0.66f %- 10.0f })
        |* SkillsetGraph(Skillsets.skills.Jack, Position = { Position.Margin(20.0f) with Top = 0.66f %+ 10.0f })
        base.Init parent