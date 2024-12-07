﻿namespace Interlude.Features.Stats

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Data.User
open Prelude.Charts.Processing.Patterns
open Interlude.UI

type RatingTile(color: Color, value: float32) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        Render.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black.O3
        Background.draw (this.Bounds, Colors.grey_2, 0.8f)
        Render.rect this.Bounds color.O1
        Text.fill_b (Style.font, sprintf "%.0f" value, this.Bounds.Shrink(10.0f, 0.0f), Colors.text, Alignment.CENTER)

type SkillsetGraph(pattern_type: CorePattern, target: PatternSkillBreakdown) =
    inherit StaticWidget(NodeType.None)

    let push = target.Push |> Array.ofList |> Array.rev
    let control = target.Control |> Array.ofList |> Array.rev
    let accuracy = target.Accuracy |> Array.ofList |> Array.rev

    let min_bpm = (Seq.concat [push; control; accuracy] |> Seq.map _.BPM |> Seq.min |> float32) - 5.0f
    let max_bpm = push |> Seq.map _.BPM |> Seq.max |> float32
    let max_duration = push |> Seq.map _.Duration |> Seq.max

    let AXIS_HEIGHT = 40.0f

    let mutable hover = false
    let mutable tooltip_bpm = 0
    let mutable tooltip: (Callout * (float32 * float32)) option = None

    let format_duration (ms: GameplayTime) =
        if ms < 1000.0f<ms / rate> then
            "short bursts"
        else sprintf "%is" (floor (ms / 1000.0f<ms / rate>) |> int)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let next_hover = Mouse.hover this.Bounds
        let bpm = (Mouse.x() - this.Bounds.Left) / this.Bounds.Width * float32 (max_bpm - min_bpm) + float32 min_bpm |> int

        if (not hover || bpm <> tooltip_bpm) && next_hover && Mouse.moved_recently () then

            tooltip_bpm <- bpm
            let content =
                Callout.Normal
                    .Title(sprintf "%i BPM %O" bpm pattern_type)
                    .Body(sprintf "Push: %s with passable acc." (PatternStatLine.get_duration_at bpm target.Push |> Option.defaultValue 0.0f<ms/rate> |> format_duration))
                    .Body(sprintf "Control: %s with good acc." (PatternStatLine.get_duration_at bpm target.Control |> Option.defaultValue 0.0f<ms/rate> |> format_duration))
                    .Body(sprintf "Accuracy: %s with high acc." (PatternStatLine.get_duration_at bpm target.Accuracy |> Option.defaultValue 0.0f<ms/rate> |> format_duration))
            tooltip <- Some (content, Callout.measure content)

        elif tooltip.IsSome && not next_hover then
            tooltip <- None

        hover <- next_hover

    override this.Draw() =
        let bottom = this.Bounds.Bottom - AXIS_HEIGHT
        let height = this.Bounds.Height - AXIS_HEIGHT

        let x bpm = this.Bounds.Left + (float32 bpm - min_bpm) / (max_bpm - min_bpm) * this.Bounds.Width
        let y d = bottom - d / max_duration * height
        let mutable last = min_bpm
        for point in push do
            Render.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), bottom)) Colors.blue
            last <- float32 point.BPM

        let mutable last = min_bpm
        for point in control do
            Render.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), bottom)) Colors.green
            last <- float32 point.BPM

        let mutable last = min_bpm
        for point in accuracy do
            Render.rect (Rect.Create(x last, y point.Duration, x (float32 point.BPM), bottom)) Colors.yellow_accent
            last <- float32 point.BPM

        Render.rect (Rect.Create(this.Bounds.Left, y 60000.0f<ms / rate> - 2.5f, this.Bounds.Right, y 60000.0f<ms / rate> + 2.5f)) Colors.white.O1

        Render.rect (this.Bounds.SliceB(AXIS_HEIGHT)) Colors.shadow_2.O1
        let SPACING = floor(max_bpm / 200.0f) * 10.0f |> max 10.0f
        let mutable bpm = round(min_bpm / SPACING) * SPACING
        while bpm < max_bpm - SPACING * 1.5f do
            bpm <- bpm + SPACING
            Render.rect (Rect.Box(x bpm, bottom, 5.0f, 7.5f).Translate(-2.5f, 0.0f)) Colors.white
            Text.draw_aligned(Style.font, sprintf "%.0f" bpm, 20.0f, x bpm, bottom + 7.5f, Colors.white, Alignment.CENTER)

        match tooltip with
        | None -> ()
        | Some (c, (width, height)) ->
            let x, y = Mouse.pos()
            let x, y = x - width * 0.5f |> max (this.Bounds.Left + 20.0f) |> min (this.Bounds.Right - width - 20.0f), y - height - 20.0f
            let b = Rect.Box(x, y, width, height)
            Render.rect b Colors.cyan_shadow.O3
            Render.rect (b.BorderL(Style.PADDING)) Colors.cyan_accent
            Render.rect (b.BorderCornersT(Style.PADDING)) Colors.cyan_accent
            Render.rect (b.BorderR(Style.PADDING)) Colors.cyan_accent
            Render.rect (b.BorderCornersB(Style.PADDING)) Colors.cyan_accent
            Callout.draw(x, y, width, height, Colors.text, c)

    static member Create(pattern_type: CorePattern, data: PatternSkillBreakdown) =
        if data = PatternSkillBreakdown.Default then
            EmptyState(Icons.X, %"stats.skillsets.empty") :> Widget
        else
            let total = PatternStatLine.value data.Push + PatternStatLine.value data.Accuracy + PatternStatLine.value data.Control
            let mult = pattern_type.RatingMultiplier
            Container(NodeType.None)
            |+ SkillsetGraph(pattern_type, data, Position = Position.ShrinkB(100.0f))
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
        let available_keymodes =
            seq {
                for i = 3 to 10 do
                    if Stats.TOTAL_STATS.KeymodeSkills.[i - 3] <> KeymodeSkillBreakdown.Default then
                        yield i
            }
            |> Array.ofSeq

        let available_keymodes = if available_keymodes.Length = 0 then [|4|] else available_keymodes

        let keymode = Setting.simple available_keymodes.[0]
        let skill = Setting.simple Jacks

        let graph_container = SwapContainer(SkillsetGraph.Create(Jacks, Stats.TOTAL_STATS.KeymodeSkills.[keymode.Value - 3].Jacks), Position = Position.Shrink(20.0f))

        let refresh_graph() =
            let skills = Stats.TOTAL_STATS.KeymodeSkills.[keymode.Value - 3]
            let skill_data =
                match skill.Value with
                | Jacks -> skills.Jacks
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
                    skill.Value <- match skill.Value with Jacks -> Chordstream | Chordstream -> Stream | Stream -> Jacks
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