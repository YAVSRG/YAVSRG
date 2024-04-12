namespace Interlude.UI

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Interlude.UI

type private Notification =
    {
        Data: Callout
        Size: float32 * float32
        Fade: Animation.Fade
        FillColor: Color * Color
        ContentColor: Color * Color
        mutable Duration: float
    }

module Notifications =

    let private displayed_notifications = ResizeArray<Notification>()

    type private Display() =
        inherit Overlay(NodeType.None)

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            let mutable y = this.Bounds.Top + 80.0f

            for i in displayed_notifications do
                let width, height = i.Size

                let bounds =
                    Rect.Box(this.Bounds.Right - width - 10.0f, y, width, height)

                Callout.update (bounds.Left, bounds.Top, width, height, i.Data)
                y <- y + (height + 15.0f) * i.Fade.Value

                i.Duration <- i.Duration - elapsed_ms
                i.Fade.Update elapsed_ms

                if i.Fade.Target <> 0.0f then
                    if i.Duration <= 0.0 then
                        i.Fade.Target <- 0.0f
                elif i.Fade.Value < 0.01f then
                    sync (fun () -> displayed_notifications.Remove i |> ignore)

        override this.Draw() =
            let mutable y = this.Bounds.Top + 80.0f

            for i in displayed_notifications do
                let width, height = i.Size
                let accent, body = i.FillColor

                let bounds =
                    Rect.Box(this.Bounds.Right - width - 10.0f, y, width, height)

                let border = bounds.Expand(5.0f)
                Draw.rect (border.SliceLeft(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect (border.SliceTop(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect (border.SliceRight(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect (border.SliceBottom(5.0f)) (accent.O4a i.Fade.Alpha)
                Draw.rect bounds (Colors.shadow_2.O2a i.Fade.Alpha)
                Draw.rect bounds (body.O3a i.Fade.Alpha)

                Callout.draw (
                    bounds.Left,
                    bounds.Top,
                    width,
                    height,
                    ((fst i.ContentColor).O4a i.Fade.Alpha, (snd i.ContentColor).O4a i.Fade.Alpha),
                    i.Data
                )

                y <- y + (height + 15.0f) * i.Fade.Value

    let display : Widget = Display()

    let private add (body: Callout, colors: Color * Color, content_colors: Color * Color) =
        ensure_ui_thread
        <| fun () ->
            let n: Notification =
                {
                    Data = body
                    Size = Callout.measure body
                    FillColor = colors
                    ContentColor = content_colors
                    Fade = Animation.Fade(0.0f, Target = 1.0f)
                    Duration = 2000.0
                }

            displayed_notifications.Add n

    let private add_long (body: Callout, colors: Color * Color, content_colors: Color * Color) =
        ensure_ui_thread
        <| fun () ->
            let n: Notification =
                {
                    Data = body
                    Size = Callout.measure body
                    FillColor = colors
                    ContentColor = content_colors
                    Fade = Animation.Fade(0.0f, Target = 1.0f)
                    Duration = 5000.0
                }

            displayed_notifications.Add n

    let task_feedback (icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.pink_accent, Colors.pink), Colors.text)
        ensure_ui_thread Style.notify_task.Play

    let action_feedback (icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.cyan_accent, Colors.cyan), Colors.text)
        ensure_ui_thread Style.notify_info.Play

    let action_feedback_button
        (
            icon: string,
            title: string,
            description: string,
            button_label: string,
            button_action: unit -> unit
        ) =
        add_long (
            Callout.Small
                .Icon(icon)
                .Title(title)
                .Body(description)
                .Button(button_label, button_action),
            (Colors.cyan_accent, Colors.cyan),
            Colors.text
        )

        ensure_ui_thread Style.notify_info.Play

    let system_feedback (icon: string, title: string, description: string) =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.green_accent, Colors.green), Colors.text)
        ensure_ui_thread Style.notify_system.Play

    let error (title, description) =
        add_long (
            Callout.Small.Icon(Icons.ALERT_CIRCLE).Title(title).Body(description),
            (Colors.red_accent, Colors.red),
            Colors.text
        )

        ensure_ui_thread Style.notify_error.Play