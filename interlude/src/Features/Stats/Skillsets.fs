namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Gameplay
open Prelude.Data
open Prelude.Charts.Processing.Patterns
open Interlude.UI

type RatingTile(color: Color, value: float32) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black.O3
        Background.draw (this.Bounds, Colors.grey_2, 0.8f)
        Draw.rect this.Bounds color.O1
        Text.fill_b (Style.font, sprintf "%.0f" value, this.Bounds.Shrink(10.0f, 0.0f), Colors.text, Alignment.CENTER)

type SkillsetGraph(target: PatternSkillBreakdown) =
    inherit StaticWidget(NodeType.None)

    let push = target.Push |> Array.ofList |> Array.rev
    let control = target.Control |> Array.ofList |> Array.rev
    let accuracy = target.Accuracy |> Array.ofList |> Array.rev

    let min_bpm = (Seq.concat [push; control; accuracy] |> Seq.map _.BPM |> Seq.min |> float32) - 5.0f
    let max_bpm = push |> Seq.map _.BPM |> Seq.max |> float32
    let max_duration = push |> Seq.map _.Duration |> Seq.max

    let AXIS_HEIGHT = 40.0f

    override this.Draw() =
        let bottom = this.Bounds.Bottom - AXIS_HEIGHT
        let height = this.Bounds.Height - AXIS_HEIGHT

        let x bpm = this.Bounds.Left + (float32 bpm - min_bpm) / (max_bpm - min_bpm) * this.Bounds.Width
        let y d = bottom - d / max_duration * height
        let mutable last = min_bpm
        for point in push do
            Draw.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), bottom)) Colors.blue
            last <- float32 point.BPM
        
        let mutable last = min_bpm
        for point in control do
            Draw.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), bottom)) Colors.green
            last <- float32 point.BPM
        
        let mutable last = min_bpm
        for point in accuracy do
            Draw.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), bottom)) Colors.yellow_accent
            last <- float32 point.BPM

        Draw.rect (Rect.Create(this.Bounds.Left, y 60000.0f<ms> - 2.5f, this.Bounds.Right, y 60000.0f<ms> + 2.5f)) Colors.white.O1

        Draw.rect (this.Bounds.SliceB(AXIS_HEIGHT)) Colors.shadow_2.O1
        let SPACING = floor(max_bpm / 200.0f) * 10.0f |> max 10.0f
        let mutable bpm = round(min_bpm / SPACING) * SPACING
        while bpm < max_bpm - SPACING * 1.5f do
            bpm <- bpm + SPACING
            Draw.rect (Rect.Box(x bpm, bottom, 5.0f, 7.5f).Translate(-2.5f, 0.0f)) Colors.white
            Text.draw_aligned(Style.font, sprintf "%.0f" bpm, 20.0f, x bpm, bottom + 7.5f, Colors.white, Alignment.CENTER)

    static member Create(pattern: CorePatternType, data: PatternSkillBreakdown) =
        if data = PatternSkillBreakdown.Default then
            EmptyState(Icons.X, "No data") :> Widget
        else
            let total = PatternStatLine.value data.Push + PatternStatLine.value data.Accuracy + PatternStatLine.value data.Control
            let mult = pattern.RatingMultiplier
            Container(NodeType.None)
            |+ SkillsetGraph(data, Position = Position.ShrinkB(100.0f))
            |+ (
                GridFlowContainer(60.0f, 4, Spacing = (90.0f, 0.0f), Position = Position.SliceB(60.0f))
                |+ RatingTile(Colors.yellow_accent, mult * PatternStatLine.value data.Accuracy)
                |+ RatingTile(Colors.green_accent, mult * PatternStatLine.value data.Control)
                |+ RatingTile(Colors.blue_accent, mult * PatternStatLine.value data.Push)
                |+ RatingTile(Colors.shadow_2, mult * total)
            )
            :> Widget

type Skills() =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        Skillsets.calculate Interlude.Content.Content.UserData Interlude.Content.Content.Library
        let available_keymodes =
            seq {
                for i = 3 to 10 do
                    if Skillsets.keymode_skills.[i - 3] <> KeymodeSkillBreakdown.Default then
                        yield i
            }
            |> Array.ofSeq

        let available_keymodes = if available_keymodes.Length = 0 then [|4|] else available_keymodes

        let keymode = Setting.simple available_keymodes.[0]
        let skill = Setting.simple Jack

        let graph_container = SwapContainer(SkillsetGraph.Create(Jack, Skillsets.keymode_skills.[keymode.Value - 3].Jack), Position = Position.Shrink(20.0f))

        let refresh_graph() =
            let skills = Skillsets.keymode_skills.[keymode.Value - 3]
            let skill_data = 
                match skill.Value with
                | Jack -> skills.Jack
                | Chordstream -> skills.Chordstream
                | Stream -> skills.Stream
            graph_container.Current <- SkillsetGraph.Create(skill.Value, skill_data)

        let keymode_switcher = 
            InlaidButton(
                (fun () -> sprintf "%iK" keymode.Value),
                (fun () ->
                    keymode.Value <- available_keymodes.[(1 + Array.findIndex ((=) keymode.Value) available_keymodes) % available_keymodes.Length]
                    refresh_graph()
                ),
                ""
            )

        let skill_switcher = 
            InlaidButton(
                (fun () -> sprintf "%O" skill.Value),
                (fun () ->
                    skill.Value <- match skill.Value with Jack -> Chordstream | Chordstream -> Stream | Stream -> Jack
                    refresh_graph()
                ),
                ""
            )

        this
        |+ (
            FlowContainer.RightToLeft(180.0f, Spacing = 10.0f, Position = Position.Shrink(20.0f).SliceT(InlaidButton.HEIGHT))
            |+ skill_switcher
            |+ keymode_switcher
        )
        |* graph_container
        base.Init parent