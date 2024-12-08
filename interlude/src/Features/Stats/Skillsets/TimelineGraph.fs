﻿namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Charts.Processing.Patterns
open Prelude.Gameplay
open Prelude.Data.User

type private GraphDataPoint =
    {
        DaysAgo: int
        Value: float32
    }

type SkillTimelineGraph(keymode: int) =
    inherit StaticWidget(NodeType.None)

    let all_time_records = Stats.TOTAL_STATS.KeymodeSkills.[keymode - 3].Tiny
    let all_time_max = Array.max [|all_time_records.Jacks; all_time_records.Chordstream; all_time_records.Stream|]

    let TODAY = Timestamp.now() |> timestamp_to_local_day |> DateOnly.FromDateTime

    let sessions =
        Stats.PREVIOUS_SESSIONS.Keys
        |> Seq.sortDescending
        |> Seq.map (fun k -> k, TODAY.DayNumber - k.DayNumber, Map.find k Stats.PREVIOUS_SESSIONS)
        |> Array.ofSeq

    let jack_data = sessions |> Array.map (fun (_, days_ago, sessions) -> { DaysAgo = days_ago; Value = sessions |> Seq.map _.KeymodeSkills.[keymode - 3].Jacks |> Seq.max })
    let chordstream_data = sessions |> Array.map (fun (_, days_ago, sessions) -> { DaysAgo = days_ago; Value = sessions |> Seq.map _.KeymodeSkills.[keymode - 3].Chordstream |> Seq.max })
    let stream_data = sessions |> Array.map (fun (_, days_ago, sessions) -> { DaysAgo = days_ago; Value = sessions |> Seq.map _.KeymodeSkills.[keymode - 3].Stream |> Seq.max })
    let date_data = sessions |> Array.map (fun (date, days_ago, _) -> date, days_ago)

    let AXIS_HEIGHT = 60.0f
    let HTHICKNESS = 2.5f
    let DROP_GRADIENT_SIZE = 80.0f
    let day_range = Animation.Fade(90.0f)

    let mutable hover_index = 0
    let mutable show_tooltip = false

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if Mouse.hover this.Bounds then
            day_range.Target <- day_range.Target - 2.0f * Mouse.scroll() |> min 365.0f |> max 30.0f

            let hover_day = (this.Bounds.Right - Mouse.x()) / this.Bounds.Width * day_range.Value |> round |> int
            while hover_index + 1 < date_data.Length && abs (snd date_data.[hover_index + 1] - hover_day) < abs (snd date_data.[hover_index] - hover_day) do
                hover_index <- hover_index + 1
            while hover_index > 0 && abs (snd date_data.[hover_index - 1] - hover_day) < abs (snd date_data.[hover_index] - hover_day) do
                hover_index <- hover_index - 1
            show_tooltip <- date_data.Length > 0
        else
            show_tooltip <- false

        day_range.Update elapsed_ms

    override this.Draw() =
        Render.rect this.Bounds Colors.shadow_2.O2

        let bottom = this.Bounds.Bottom - AXIS_HEIGHT
        let height = this.Bounds.Height - AXIS_HEIGHT - 5.0f

        // GRAPH
        let x day = this.Bounds.Right - (float32 day / day_range.Value) * this.Bounds.Width
        let y d = bottom - d / all_time_max * height

        let draw_graph (color: Color) (data: GraphDataPoint array) =
            let mutable i = 0
            let mutable previous_x = 0.0f
            let mutable previous_y = 0.0f
            let mutable previous_day = -31
            while i < data.Length do
                let x, y = x data.[i].DaysAgo, y data[i].Value

                if data.[i].DaysAgo - previous_day <= 30 then
                    let theta = MathF.Atan((previous_y - y) / (previous_x - x))
                    let dy = -HTHICKNESS * MathF.Cos theta
                    let dx = HTHICKNESS * MathF.Sin theta
                    Render.quad
                        <| Quad.createv (x, y) (previous_x, previous_y) (previous_x, previous_y + DROP_GRADIENT_SIZE) (x, y + DROP_GRADIENT_SIZE)
                        <| Quad.gradient_top_to_bottom color.O1 color.O0

                    Render.quad
                        <| Quad.createv (x + dx, y + dy) (previous_x + dx, previous_y + dy) (previous_x - dx, previous_y - dy) (x - dx, y - dy)
                        <| color.AsQuad

                Render.quad
                    <| Quad.createv (x, y - 7.5f) (x + 7.5f, y) (x, y + 7.5f) (x - 7.5f, y)
                    <| (if i = hover_index && show_tooltip then Colors.white else color).AsQuad

                previous_x <- x
                previous_y <- y
                previous_day <- data[i].DaysAgo
                if float32 data.[i].DaysAgo > day_range.Value then i <- data.Length
                i <- i + 1

        Render.stencil_create false
        Render.rect this.Bounds Color.Transparent
        Render.stencil_begin_draw()
        draw_graph Colors.red_accent jack_data
        draw_graph Colors.green_accent chordstream_data
        draw_graph Colors.cyan_accent stream_data
        Render.stencil_finish()

        let inline draw_tooltip () =
            let date, days_ago = date_data.[hover_index]
            let jack_rating = jack_data.[hover_index].Value
            let chordstream_rating = chordstream_data.[hover_index].Value
            let stream_rating = stream_data.[hover_index].Value
            let x = this.Bounds.Right - (float32 days_ago / day_range.Value) * this.Bounds.Width
            let y = bottom - Array.max [|jack_rating; chordstream_rating; stream_rating|] / all_time_max * height
            let box = Rect.Create(x - 180.0f, y - 220.0f, x + 180.0f, y - 20.0f)
            Render.rect (box.Expand(Style.PADDING)) Colors.cyan
            Render.rect box Colors.shadow_2.O3
            Text.fill_b(Style.font, date.ToLongDateString(), box.SliceT(45.0f), Colors.text, Alignment.CENTER)
            Text.fill_b(Style.font, sprintf "%O %.0f" Jacks jack_rating, box.ShrinkT(45.0f).SliceT(40.0f), Colors.text_red, Alignment.CENTER)
            Text.fill_b(Style.font, sprintf "%O %.0f" Chordstream chordstream_rating, box.ShrinkT(85.0f).SliceT(40.0f), Colors.text_green, Alignment.CENTER)
            Text.fill_b(Style.font, sprintf "%O %.0f" Stream stream_rating, box.ShrinkT(125.0f).SliceT(40.0f), Colors.text_cyan, Alignment.CENTER)
            Text.fill_b(Style.font, "Click to view", box.SliceB(35.0f), Colors.text_subheading, Alignment.CENTER)
            // todo: click to view

        if show_tooltip then
            draw_tooltip()