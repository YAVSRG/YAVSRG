namespace Interlude.UI

open System.Runtime.CompilerServices
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Windowing
open Prelude
open Interlude.UI

type DisplayedNotification =
    private
        {
            Data: Callout
            Size: float32 * float32
            Fade: Animation.Fade
            FillColor: Color * Color
            ContentColor: Color * Color
            mutable Duration: float
        }

    static member FromNotification(notification: Notification) : DisplayedNotification =
        let body =
            Callout.Small.Icon(notification.Icon).Title(notification.Title).Body(notification.Description)

        let color =
            match notification.Type with
            | NotificationType.Task -> (Colors.pink_accent, Colors.pink)
            | NotificationType.System -> (Colors.green_accent, Colors.green)
            | NotificationType.Error -> (Colors.red_accent, Colors.red)
            | NotificationType.Result _ -> (Colors.cyan_accent, Colors.cyan)

        match notification.Type with
        | NotificationType.Result(Some(button_label, button_action)) ->
            let body = body.Button(button_label, button_action)

            {
                Data = body
                Size = Callout.measure body
                FillColor = color
                ContentColor = Colors.text
                Fade = Animation.Fade(0.0f, Target = 1.0f)
                Duration = 5000.0
            }
        | _ ->
            {
                Data = body
                Size = Callout.measure body
                FillColor = color
                ContentColor = Colors.text
                Fade = Animation.Fade(0.0f, Target = 1.0f)
                Duration = 2000.0
            }

    [<Extension>]
    static member ToDisplay(notification: Notification) : DisplayedNotification =
        DisplayedNotification.FromNotification(notification)

[<Sealed>]
type NotificationsDisplay() =
    inherit Overlay(NodeType.None)

    let displayed_notifications = ResizeArray<DisplayedNotification>()

    override this.Init(parent: Widget) : unit =
        base.Init(parent)

        let on_notification_added (notification: Notification) =
            displayed_notifications.Add(notification.ToDisplay())

            match notification.Type with
            | NotificationType.Error -> Style.notify_error.Play()
            | NotificationType.Result _ -> Style.notify_info.Play()
            | NotificationType.System -> Style.notify_system.Play()
            | NotificationType.Task -> Style.notify_task.Play()

        Notifications.OnNotificationAdded.Add(on_notification_added)

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        base.Update(elapsed_ms, moved)

        let mutable y = this.Bounds.Top + 80.0f

        for i in displayed_notifications do
            let width, height = i.Size

            let bounds = Rect.FromSize(this.Bounds.Right - width - 10.0f, y, width, height)

            Callout.update(bounds.Left, bounds.Top, width, height, i.Data)

            if Mouse.hover bounds then
                Input.finish_frame_events()

            y <- y + (height + 15.0f) * i.Fade.Value

            i.Duration <- i.Duration - elapsed_ms
            i.Fade.Update(elapsed_ms)

            if i.Fade.Target <> 0.0f then
                if i.Duration <= 0.0 then
                    i.Fade.Target <- 0.0f
            elif i.Fade.Value < 0.01f then
                GameThread.defer(fun () -> displayed_notifications.Remove(i) |> ignore)

    override this.Draw() : unit =
        let mutable y = this.Bounds.Top + 80.0f

        for i in displayed_notifications do
            let width, height = i.Size
            let accent, body = i.FillColor

            let bounds = Rect.FromSize(this.Bounds.Right - width - 10.0f, y, width, height)

            Render.border Style.PADDING bounds (accent.O4a(i.Fade.Alpha))
            Render.rect bounds (Colors.shadow_2.O2a(i.Fade.Alpha))
            Render.rect bounds (body.O3a(i.Fade.Alpha))

            Callout.draw(
                bounds.Left,
                bounds.Top,
                width,
                height,
                ((fst i.ContentColor).O4a i.Fade.Alpha, (snd i.ContentColor).O4a i.Fade.Alpha),
                i.Data
            )

            y <- y + (height + 15.0f) * i.Fade.Value
