namespace Interlude.UI

open System.Runtime.CompilerServices
open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Interlude.UI

[<RequireQualifiedAccess>]
type NotificationType =
    private
    | Task
    | System
    | Error
    | Result of button: (string * (unit -> unit)) option

type Notification =
    {
        Icon: string
        Title: string
        Description: string
        Type: NotificationType
    }

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

module Notifications =

    let displayed_notifications = ResizeArray<DisplayedNotification>()

    let private add_notification (notification: Notification) : unit =
        let displayed = notification.ToDisplay()
        GameThread.on_game_thread(fun () -> displayed_notifications.Add(displayed))

    let private add (body: Callout, colors: Color * Color, content_colors: Color * Color) : unit =
        GameThread.on_game_thread(fun () ->
            let n: DisplayedNotification =
                {
                    Data = body
                    Size = Callout.measure body
                    FillColor = colors
                    ContentColor = content_colors
                    Fade = Animation.Fade(0.0f, Target = 1.0f)
                    Duration = 2000.0
                }

            displayed_notifications.Add(n)
        )

    let private add_long (body: Callout, colors: Color * Color, content_colors: Color * Color) : unit =
        GameThread.on_game_thread(fun () ->
            let n: DisplayedNotification =
                {
                    Data = body
                    Size = Callout.measure body
                    FillColor = colors
                    ContentColor = content_colors
                    Fade = Animation.Fade(0.0f, Target = 1.0f)
                    Duration = 5000.0
                }

            displayed_notifications.Add(n)
        )

    let task_feedback (icon: string, title: string, description: string) : unit =
        add_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Task
            }
        )

        GameThread.on_game_thread Style.notify_task.Play

    let action_feedback (icon: string, title: string, description: string) : unit =
        add_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Result(None)
            }
        )

        add(Callout.Small.Icon(icon).Title(title).Body(description), (Colors.cyan_accent, Colors.cyan), Colors.text)
        GameThread.on_game_thread Style.notify_info.Play

    let action_feedback_button
        (icon: string, title: string, description: string, button_label: string, button_action: unit -> unit)
        : unit =
        add_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Result(Some(button_label, button_action))
            }
        )

        GameThread.on_game_thread Style.notify_info.Play

    let system_feedback (icon: string, title: string, description: string) : unit =
        add_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.System
            }
        )

        GameThread.on_game_thread Style.notify_system.Play

    let error (title: string, description: string) : unit =
        add_notification(
            {
                Icon = Icons.ALERT_CIRCLE
                Title = title
                Description = description
                Type = NotificationType.Error
            }
        )

        GameThread.on_game_thread Style.notify_error.Play
