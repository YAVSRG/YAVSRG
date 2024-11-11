namespace Interlude.Features.Stats

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Prelude
open Prelude.Data.User
open Interlude.Content
open Interlude.UI

type ActivityFeed(selected: Setting<DateOnly>, on_day_selected: Session array -> unit) =
    inherit StaticWidget(NodeType.None)

    let session_dates = Stats.calculate Content.Library Content.UserData |> Array.groupBy (_.Start >> timestamp_to_local_day >> DateOnly.FromDateTime) |> Map.ofArray

    let today, day_of_week = 
        let today_datetime = Timestamp.now() |> timestamp_to_local_day
        DateOnly.FromDateTime(today_datetime), today_datetime.DayOfWeek

    let mutable hovered_day = None

    override this.Draw() =
        Draw.rect this.Bounds Colors.shadow_2.O2

        let box_size = (this.Bounds.Height - 20.0f) / 7.0f
        let weeks_shown = (this.Bounds.Width - 20.0f) / box_size |> floor |> int
        let x_padding = (this.Bounds.Width - (box_size * float32 weeks_shown)) * 0.5f

        let mutable day = today.AddDays(- int day_of_week - (weeks_shown - 1) * 7)
        let mutable i = 0
        while day <= today do
            
            let pos = Rect.Box(this.Bounds.Left + x_padding + float32 (i / 7) * box_size, this.Bounds.Top + 10.0f + float32 (i % 7) * box_size, box_size, box_size)

            let color = 
                match session_dates.TryFind day with
                | Some sessions when sessions.Length = 1 -> Colors.cyan
                | Some _ -> Colors.cyan_accent
                | None -> Colors.black

            if day = selected.Value then
                Draw.rect pos Colors.white.O2
            elif Some day = hovered_day then
                Draw.rect pos Colors.yellow_accent.O2

            Draw.rect (pos.ShrinkPercent(0.2f)) color

            i <- i + 1
            day <- day.AddDays 1

        match hovered_day with
        | Some d ->
            Text.draw_aligned_b(Style.font, d.ToString("dddd, dd MMMM yyyy"), 20.0f, this.Bounds.Right - 10.0f, this.Bounds.Top - 35.0f, Colors.text_subheading, Alignment.RIGHT)
        | None -> ()
        Text.draw_b(Style.font, "Activity", 30.0f, this.Bounds.Left + 15.0f, this.Bounds.Top - 50.0f, Colors.text)
        Text.draw_b(Style.font, Icons.ARROW_LEFT + " View older", 20.0f, this.Bounds.Left + 15.0f, this.Bounds.Bottom + 5.0f, Colors.text)
    
    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        hovered_day <- None

        let box_size = (this.Bounds.Height - 20.0f) / 7.0f
        let weeks_shown = (this.Bounds.Width - 20.0f) / box_size |> floor |> int
        let x_padding = (this.Bounds.Width - (box_size * float32 weeks_shown)) * 0.5f

        let mutable day = today.AddDays(- int day_of_week - (weeks_shown - 1) * 7)
        let mutable i = 0
        while day <= today do
            
            let pos = Rect.Box(this.Bounds.Left + x_padding + float32 (i / 7) * box_size, this.Bounds.Top + 10.0f + float32 (i % 7) * box_size, box_size, box_size)

            if Mouse.hover pos then 
                hovered_day <- Some day

                if Mouse.left_click() then
                    match session_dates.TryFind day with
                    | Some sessions -> 
                        selected.Set day
                        on_day_selected sessions
                    | _ -> ()

                day <- today.AddDays 1

            i <- i + 1
            day <- day.AddDays 1