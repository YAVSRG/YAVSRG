namespace Interlude.Features.Stats

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Data.User.Stats
open Prelude.Calculator
open Prelude.Calculator.Patterns
open Interlude.UI
open Interlude.Content

type GraphSource =
    | AllTime
    | Recent
    | Session // not used; // todo: store fast-decaying and slow-decaying skills separately in sessions
    override this.ToString() =
        match this with
        | AllTime -> "All time"
        | Recent -> "Last 2 weeks"
        | Session -> "This session"

type SkillBreakdownGraph(pattern_type: CorePattern, source: GraphSource, data: PatternSkillBreakdown) =
    inherit StaticWidget(NodeType.None)

    let mult = pattern_type.RatingMultiplier
    let total_rating = (PatternStatLine.value data.Push + PatternStatLine.value data.Control + PatternStatLine.value data.Accuracy) * mult
    let push_rating = mult * PatternStatLine.value data.Push
    let control_rating = mult * PatternStatLine.value data.Control
    let accuracy_rating = mult * PatternStatLine.value data.Accuracy

    let push = data.Push |> Array.ofList |> Array.rev
    let control = data.Control |> Array.ofList |> Array.rev
    let accuracy = data.Accuracy |> Array.ofList |> Array.rev

    let min_bpm = (Seq.concat [push; control; accuracy] |> Seq.map _.BPM |> Seq.min |> float32) - 5.0f
    let max_bpm = push |> Seq.map _.BPM |> Seq.max |> float32
    let max_duration = push |> Seq.map _.Duration |> Seq.max |> max 5000.0f<ms / rate>

    let AXIS_HEIGHT = 60.0f

    let mutable hover = false
    let mutable tooltip_bpm = 0
    let mutable tooltip: (Callout * (float32 * float32)) option = None

    let format_duration (ms: GameplayTime) =
        if ms < 1000.0f<ms / rate> then
            "short bursts"
        else sprintf "%is" (floor (ms / 1000.0f<ms / rate>) |> int)

    let threshold_a, threshold_c, threshold_p = pattern_type.AccuracyBreakpoints
    let a_accuracy_label =
        let base_label = sprintf "%g%% %s" (threshold_a * 100.0) SC_J4.Name
        if Rulesets.current_hash <> SC_J4_HASH then
            match RulesetComparison.compare (threshold_a * 100.0) SC_J4 Rulesets.current with
            | Some cmp -> sprintf "%s (~= %.1f%% %s)" base_label cmp.Average Rulesets.current.Name
            | None -> base_label
        else base_label

    let c_accuracy_label =
        let base_label = sprintf "%g%% %s" (threshold_c * 100.0) SC_J4.Name
        if Rulesets.current_hash <> SC_J4_HASH then
            match RulesetComparison.compare (threshold_c * 100.0) SC_J4 Rulesets.current with
            | Some cmp -> sprintf "%s (~= %.1f%% %s)" base_label cmp.Average Rulesets.current.Name
            | None -> base_label
        else base_label

    let p_accuracy_label =
        let base_label = sprintf "%g%% %s" (threshold_p * 100.0) SC_J4.Name
        if Rulesets.current_hash <> SC_J4_HASH then
            match RulesetComparison.compare (threshold_p * 100.0) SC_J4 Rulesets.current with
            | Some cmp -> sprintf "%s (~= %.1f%% %s)" base_label cmp.Average Rulesets.current.Name
            | None -> base_label
        else base_label

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        let next_hover = Mouse.hover this.Bounds
        let bpm = (Mouse.x() - this.Bounds.Left) / this.Bounds.Width * float32 (max_bpm - min_bpm) + float32 min_bpm |> int

        if (not hover || bpm <> tooltip_bpm) && next_hover && Mouse.moved_recently () then

            tooltip_bpm <- bpm
            let content =
                Callout.Small
                    .Title(sprintf "%i BPM %O" bpm pattern_type)
                    .Body(sprintf "Push: %s @ %s" (PatternStatLine.get_duration_at bpm data.Push |> Option.defaultValue 0.0f<ms/rate> |> format_duration) p_accuracy_label)
                    .Body(sprintf "Control: %s @ %s" (PatternStatLine.get_duration_at bpm data.Control |> Option.defaultValue 0.0f<ms/rate> |> format_duration) c_accuracy_label)
                    .Body(sprintf "Accuracy: %s @ %s" (PatternStatLine.get_duration_at bpm data.Accuracy |> Option.defaultValue 0.0f<ms/rate> |> format_duration) a_accuracy_label)
            tooltip <- Some (content, Callout.measure content)

        elif tooltip.IsSome && not next_hover then
            tooltip <- None

        hover <- next_hover

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O2

        let bottom = this.Bounds.Bottom - AXIS_HEIGHT
        let height = this.Bounds.Height - AXIS_HEIGHT

        // GRAPH
        let x bpm = this.Bounds.Left + (float32 bpm - min_bpm) / (max_bpm - min_bpm) * this.Bounds.Width
        let y d = bottom - d / max_duration * height
        let mutable last = min_bpm
        for point in push do
            Render.rect_edges
                (x last)
                (y point.Duration)
                (x (float32 point.BPM))
                bottom
                Colors.blue
            last <- float32 point.BPM

        let mutable last = min_bpm
        for point in control do
            Render.rect_edges
                (x last)
                (y point.Duration)
                (x (float32 point.BPM))
                bottom
                Colors.green
            last <- float32 point.BPM

        let mutable last = min_bpm
        for point in accuracy do
            Render.rect_edges
                (x last)
                (y point.Duration)
                (x (float32 point.BPM))
                bottom
                Colors.yellow_accent
            last <- float32 point.BPM

        if max_duration > 60000.0f<ms / rate> then
            Render.rect_edges
                this.Bounds.Left
                (y 60000.0f<ms / rate> - 2.5f)
                this.Bounds.Right
                (y 60000.0f<ms / rate> + 2.5f)
                Colors.white.O1

        // X AXIS
        Render.rect (this.Bounds.SliceB(AXIS_HEIGHT).SliceT(2.5f)) Colors.white
        let SPACING = floor(max_bpm / 200.0f) * 10.0f |> max 10.0f
        let mutable bpm = floor(min_bpm / SPACING) * SPACING
        while bpm < max_bpm - SPACING * 1.5f do
            bpm <- bpm + SPACING
            Render.rect (Rect.FromSize(x bpm, bottom, 5.0f, 7.5f).Translate(-2.5f, 0.0f)) Colors.white
            Text.draw_aligned(Style.font, sprintf "%.0f" bpm, 15.0f, x bpm, bottom + 7.5f, Colors.grey_1, Alignment.CENTER)
        Text.draw_aligned(Style.font, "BPM", 18.0f, this.Bounds.CenterX, bottom + 32.5f, Colors.white, Alignment.CENTER)

        // todo: a Y-axis

        // KEY
        Text.fill_b(Style.font, sprintf "%O rating: %.0f" pattern_type total_rating, this.Bounds.Shrink(10.0f).SliceT(50.0f).ShrinkR(10.0f), Colors.text, Alignment.RIGHT)

        Text.fill_b(Style.font, sprintf "Push rating: %.0f" push_rating, this.Bounds.Shrink(10.0f).ShrinkT(50.0f).SliceT(35.0f).ShrinkR(35.0f), Colors.text_subheading, Alignment.RIGHT)
        Render.rect (this.Bounds.Shrink(10.0f).ShrinkT(50.0f).SliceT(35.0f).SliceR(35.0f).Shrink(10.0f)) Colors.blue_accent

        Text.fill_b(Style.font, sprintf "Control rating: %.0f" control_rating, this.Bounds.Shrink(10.0f).ShrinkT(85.0f).SliceT(35.0f).ShrinkR(35.0f), Colors.text_subheading, Alignment.RIGHT)
        Render.rect (this.Bounds.Shrink(10.0f).ShrinkT(85.0f).SliceT(35.0f).SliceR(35.0f).Shrink(10.0f)) Colors.green_accent

        Text.fill_b(Style.font, sprintf "Accuracy rating: %.0f" accuracy_rating, this.Bounds.Shrink(10.0f).ShrinkT(120.0f).SliceT(35.0f).ShrinkR(35.0f), Colors.text_subheading, Alignment.RIGHT)
        Render.rect (this.Bounds.Shrink(10.0f).ShrinkT(120.0f).SliceT(35.0f).SliceR(35.0f).Shrink(10.0f)) Colors.yellow_accent

        // TOOLTIP
        match tooltip with
        | None -> ()
        | Some (c, (width, height)) ->
            let x, y = Mouse.pos()
            let x, y = x - width * 0.5f |> max (this.Bounds.Left + 20.0f) |> min (this.Bounds.Right - width - 20.0f), y - height - 20.0f
            let b = Rect.FromSize(x, y, width, height)
            Render.border Style.PADDING b Colors.cyan_accent
            Render.rect b Colors.cyan_shadow
            Callout.draw(x, y, width, height, Colors.text, c)

    static member Create(keymode: int, pattern_type: CorePattern, source: GraphSource) =
        let keymode_info =
            match source with
            | AllTime -> TOTAL_STATS.KeymodeSkills.[keymode - 3]
            | Recent -> CURRENT_SESSION.KeymodeSkills.[keymode - 3]
            | Session -> CURRENT_SESSION.KeymodeSkills.[keymode - 3]
        let data =
            match pattern_type with
            | Jacks -> keymode_info.Jacks
            | Chordstream -> keymode_info.Chordstream
            | Stream -> keymode_info.Stream

        if data = PatternSkillBreakdown.Default then
            EmptyState(Icons.X, %"stats.skillsets.empty") :> Widget
        else
            SkillBreakdownGraph(pattern_type, source, data)