namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Calculator.Patterns
open Prelude.Data.User.Stats

[<Struct>]
type private GraphDataPoint =
    {
        DaysAgo: int
        Value: float32
    }

module SkillTimelineGraph =

    let view_date_ev = Event<DateOnly>()
    let on_view_date = view_date_ev.Publish

type SkillTimelineGraph(keymode: int, day_range: Animation.Fade, day_offset: Animation.Fade) =
    inherit StaticWidget(NodeType.None)

    let all_time_records = TOTAL_STATS.KeymodeSkills.[keymode - 3].Tiny
    let all_time_max = Array.max [|all_time_records.Jacks; all_time_records.Chordstream; all_time_records.Stream|]

    let TODAY = Timestamp.now() |> timestamp_to_rg_calendar_day |> DateOnly.FromDateTime

    let sessions =
        PREVIOUS_SESSIONS.Keys
        |> Seq.sortDescending
        |> Seq.map (fun k -> k, TODAY.DayNumber - k.DayNumber, Map.find k PREVIOUS_SESSIONS)
        |> Array.ofSeq

    let timeline_end =
        if sessions.Length > 0 then
            let date, _, _ = Array.last sessions
            TODAY.DayNumber - date.DayNumber + 7 |> float32
        else
            187.0f

    let jack_data = sessions |> Array.map (fun (_, days_ago, sessions) -> { DaysAgo = days_ago; Value = sessions |> Seq.map _.KeymodeSkills.[keymode - 3].Jacks |> Seq.max })
    let chordstream_data = sessions |> Array.map (fun (_, days_ago, sessions) -> { DaysAgo = days_ago; Value = sessions |> Seq.map _.KeymodeSkills.[keymode - 3].Chordstream |> Seq.max })
    let stream_data = sessions |> Array.map (fun (_, days_ago, sessions) -> { DaysAgo = days_ago; Value = sessions |> Seq.map _.KeymodeSkills.[keymode - 3].Stream |> Seq.max })
    let date_data = sessions |> Array.map (fun (date, days_ago, _) -> date, days_ago)

    let AXIS_HEIGHT = 60.0f
    let HTHICKNESS = 2.5f
    let DROP_GRADIENT_SIZE = 80.0f

    let mutable hover_index = 0
    let mutable show_tooltip = false

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Mouse.hover this.Bounds then
            day_range.Target <- day_range.Target - 2.0f * Mouse.scroll() |> min 390.0f |> max 30.0f

            let hover_day = (this.Bounds.Right - Mouse.x()) / this.Bounds.Width * day_range.Value + day_offset.Value |> round |> int
            while hover_index + 1 < date_data.Length && abs (snd date_data.[hover_index + 1] - hover_day) < abs (snd date_data.[hover_index] - hover_day) do
                hover_index <- hover_index + 1
            while hover_index > 0 && abs (snd date_data.[hover_index - 1] - hover_day) < abs (snd date_data.[hover_index] - hover_day) do
                hover_index <- hover_index - 1
            show_tooltip <- date_data.Length > 0

            if show_tooltip && Mouse.left_clicked() then
                SkillTimelineGraph.view_date_ev.Trigger(fst date_data.[hover_index])

            if (%%"left").Held() then
                day_offset.Target <- day_offset.Target + float32 elapsed_ms * 0.001f * day_range.Target
            elif (%%"right").Held() then
                day_offset.Target <- day_offset.Target - float32 elapsed_ms * 0.001f * day_range.Target |> max 0.0f

        else
            show_tooltip <- false

        let distance_to_end = timeline_end - (day_offset.Target + day_range.Target)
        if distance_to_end < 0.0f then
            day_offset.Target <- day_offset.Target + distance_to_end

        day_range.Update elapsed_ms
        day_offset.Update elapsed_ms

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O2

        let bottom = this.Bounds.Bottom - AXIS_HEIGHT
        let height = this.Bounds.Height - AXIS_HEIGHT - 5.0f

        // GRAPH
        let x day = this.Bounds.Right - ((float32 day - day_offset.Value) / day_range.Value) * this.Bounds.Width
        let y d = bottom - d / all_time_max * height

        let draw_graph (color: Color) (data: GraphDataPoint array) =
            let mutable i = 0
            let mutable previous_x = 0.0f
            let mutable previous_y = 0.0f
            let mutable previous_day = -31
            while i < data.Length do
                let x, y = x data.[i].DaysAgo, y data.[i].Value

                if data.[i].DaysAgo - previous_day <= 30 then
                    let theta = MathF.Atan((previous_y - y) / (previous_x - x))
                    let dy = -HTHICKNESS * MathF.Cos theta
                    let dx = HTHICKNESS * MathF.Sin theta
                    Render.quad_points_c
                        (x, y)
                        (previous_x, previous_y)
                        (previous_x, previous_y + DROP_GRADIENT_SIZE)
                        (x, y + DROP_GRADIENT_SIZE)
                        (Quad.gradient_top_to_bottom color.O1 color.O0)

                    Render.quad_points
                        (x + dx, y + dy)
                        (previous_x + dx, previous_y + dy)
                        (previous_x - dx, previous_y - dy)
                        (x - dx, y - dy)
                        color

                Render.quad_points
                    (previous_x, previous_y - 7.5f)
                    (previous_x + 7.5f, previous_y)
                    (previous_x, previous_y + 7.5f)
                    (previous_x - 7.5f, previous_y)
                    (if i - 1 = hover_index && show_tooltip then Colors.white else color)

                previous_x <- x
                previous_y <- y
                previous_day <- data.[i].DaysAgo
                if float32 data.[i].DaysAgo - day_offset.Value > day_range.Value then i <- data.Length
                i <- i + 1

        Render.stencil_create false
        Render.rect (this.Bounds.ShrinkB AXIS_HEIGHT) Color.Transparent
        Render.stencil_begin_draw()
        draw_graph Colors.red_accent jack_data
        draw_graph Colors.green_accent chordstream_data
        draw_graph Colors.cyan_accent stream_data
        Render.stencil_finish()

        // X AXIS
        Render.stencil_create false
        Render.rect (this.Bounds.SliceB AXIS_HEIGHT) Color.Transparent
        Render.stencil_begin_draw()
        Render.rect (this.Bounds.SliceB(AXIS_HEIGHT).SliceT(2.5f)) Colors.white
        if day_range.Value > 180.0f then
            let mutable d = TODAY.AddDays(1 - TODAY.Day).AddMonths(1) // start of month
            while float32 (TODAY.DayNumber - d.DayNumber) - day_offset.Value < day_range.Value do
                d <- d.AddMonths(-1)
                let x = x (TODAY.DayNumber - d.DayNumber)
                Render.rect (Rect.FromSize(x, bottom, 5.0f, 7.5f).Translate(-2.5f, 0.0f)) Colors.white
                Text.draw_aligned(Style.font, d.ToString("MMM yy"), 15.0f, x, bottom + 7.5f, Colors.grey_1, Alignment.CENTER)
        elif day_range.Value > 90.0f then
            let mutable d = TODAY.AddDays(7 - int TODAY.DayOfWeek) // start of week
            while float32 (TODAY.DayNumber - d.DayNumber) - day_offset.Value < day_range.Value do
                d <- d.AddDays(-14)
                let x = x (TODAY.DayNumber - d.DayNumber)
                Render.rect (Rect.FromSize(x, bottom, 5.0f, 7.5f).Translate(-2.5f, 0.0f)) Colors.white
                Text.draw_aligned(Style.font, d.ToString("dd/MM/yy"), 15.0f, x, bottom + 7.5f, Colors.grey_1, Alignment.CENTER)
        else
            let mutable d = TODAY.AddDays(7 - int TODAY.DayOfWeek) // start of week
            while float32 (TODAY.DayNumber - d.DayNumber) - day_offset.Value < day_range.Value do
                d <- d.AddDays(-7)
                let x = x (TODAY.DayNumber - d.DayNumber)
                Render.rect (Rect.FromSize(x, bottom, 5.0f, 7.5f).Translate(-2.5f, 0.0f)) Colors.white
                Text.draw_aligned(Style.font, d.ToString("dd/MM/yy"), 15.0f, x, bottom + 7.5f, Colors.grey_1, Alignment.CENTER)

        Render.stencil_finish()

        // todo: a Y-axis

        let inline draw_tooltip () =
            let date, days_ago = date_data.[hover_index]
            let jack_rating = jack_data.[hover_index].Value
            let chordstream_rating = chordstream_data.[hover_index].Value
            let stream_rating = stream_data.[hover_index].Value
            let x = x days_ago |> max (this.Bounds.Left + 180.0f + 20.0f) |> min (this.Bounds.Right - 180.0f - 20.0f)
            let y = bottom - Array.max [|jack_rating; chordstream_rating; stream_rating|] / all_time_max * height
            let box = Rect.FromEdges(x - 180.0f, y - 220.0f, x + 180.0f, y - 20.0f)
            Render.rect (box.Expand(Style.PADDING)) Colors.cyan
            Render.rect box Colors.shadow_2.O3
            Text.fill_b(Style.font, date.ToLongDateString(), box.SliceT(45.0f), Colors.text, Alignment.CENTER)
            Text.fill_b(Style.font, sprintf "%O %.0f" Jacks jack_rating, box.ShrinkT(45.0f).SliceT(40.0f), Colors.text_red, Alignment.CENTER)
            Text.fill_b(Style.font, sprintf "%O %.0f" Chordstream chordstream_rating, box.ShrinkT(85.0f).SliceT(40.0f), Colors.text_green, Alignment.CENTER)
            Text.fill_b(Style.font, sprintf "%O %.0f" Stream stream_rating, box.ShrinkT(125.0f).SliceT(40.0f), Colors.text_cyan, Alignment.CENTER)
            Text.fill_b(Style.font, "Click to view", box.SliceB(35.0f), Colors.text_subheading, Alignment.CENTER)

        if show_tooltip then
            draw_tooltip()