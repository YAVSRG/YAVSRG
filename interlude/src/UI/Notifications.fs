namespace Interlude.UI

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Windowing
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
                    Rect.FromSize(this.Bounds.Right - width - 10.0f, y, width, height)

                Callout.update (bounds.Left, bounds.Top, width, height, i.Data)

                if Mouse.hover bounds then
                    Input.finish_frame_events()

                y <- y + (height + 15.0f) * i.Fade.Value

                i.Duration <- i.Duration - elapsed_ms
                i.Fade.Update elapsed_ms

                if i.Fade.Target <> 0.0f then
                    if i.Duration <= 0.0 then
                        i.Fade.Target <- 0.0f
                elif i.Fade.Value < 0.01f then
                    GameThread.defer (fun () -> displayed_notifications.Remove i |> ignore)

        override this.Draw() =
            let mutable y = this.Bounds.Top + 80.0f

            for i in displayed_notifications do
                let width, height = i.Size
                let accent, body = i.FillColor

                let bounds =
                    Rect.FromSize(this.Bounds.Right - width - 10.0f, y, width, height)

                Render.border Style.PADDING bounds (accent.O4a i.Fade.Alpha)
                Render.rect bounds (Colors.shadow_2.O2a i.Fade.Alpha)
                Render.rect bounds (body.O3a i.Fade.Alpha)

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

    let private add (body: Callout, colors: Color * Color, content_colors: Color * Color) : unit =
        GameThread.on_game_thread
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

    let private add_long (body: Callout, colors: Color * Color, content_colors: Color * Color) : unit =
        GameThread.on_game_thread
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

    let task_feedback (icon: string, title: string, description: string) : unit =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.pink_accent, Colors.pink), Colors.text)
        GameThread.on_game_thread Style.notify_task.Play

    let action_feedback (icon: string, title: string, description: string) : unit =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.cyan_accent, Colors.cyan), Colors.text)
        GameThread.on_game_thread Style.notify_info.Play

    let action_feedback_button
        (
            icon: string,
            title: string,
            description: string,
            button_label: string,
            button_action: unit -> unit
        ) : unit =
        add_long (
            Callout.Small
                .Icon(icon)
                .Title(title)
                .Body(description)
                .Button(button_label, button_action),
            (Colors.cyan_accent, Colors.cyan),
            Colors.text
        )

        GameThread.on_game_thread Style.notify_info.Play

    let system_feedback (icon: string, title: string, description: string) : unit =
        add (Callout.Small.Icon(icon).Title(title).Body(description), (Colors.green_accent, Colors.green), Colors.text)
        GameThread.on_game_thread Style.notify_system.Play

    let error (title: string, description: string) : unit =
        add_long (
            Callout.Small.Icon(Icons.ALERT_CIRCLE).Title(title).Body(description),
            (Colors.red_accent, Colors.red),
            Colors.text
        )

        GameThread.on_game_thread Style.notify_error.Play