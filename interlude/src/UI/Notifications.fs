namespace Interlude.UI

open Percyqaz.Flux.Windowing
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

type Notifications =

    static let notification_ev = Event<Notification>()

    static member internal Send(notification: Notification) : unit =
        GameThread.on_game_thread(fun () -> notification_ev.Trigger(notification))

    static member val OnNotificationAdded = notification_ev.Publish

    static member Task(icon: string, title: string, description: string) : unit =
        Notifications.Send(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Task
            }
        )

    static member Result(icon: string, title: string, description: string) : unit =
        Notifications.Send(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Result(None)
            }
        )

    static member action_feedback_button
        (icon: string, title: string, description: string, button_label: string, button_action: unit -> unit)
        : unit =
        Notifications.Send(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Result(Some(button_label, button_action))
            }
        )

    static member System(icon: string, title: string, description: string) : unit =
        Notifications.Send(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.System
            }
        )

    static member Error(title: string, description: string) : unit =
        Notifications.Send(
            {
                Icon = Icons.ALERT_CIRCLE
                Title = title
                Description = description
                Type = NotificationType.Error
            }
        )
