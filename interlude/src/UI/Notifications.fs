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

module Notifications =

    let notification_ev = Event<Notification>()
    let OnNotificationAdded = notification_ev.Publish

    let private send_notification (notification: Notification) : unit =
        GameThread.on_game_thread(fun () -> notification_ev.Trigger(notification))

    let Task (icon: string, title: string, description: string) : unit =
        send_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Task
            }
        )

    let Result (icon: string, title: string, description: string) : unit =
        send_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Result(None)
            }
        )

    let action_feedback_button
        (icon: string, title: string, description: string, button_label: string, button_action: unit -> unit)
        : unit =
        send_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Result(Some(button_label, button_action))
            }
        )

    let System (icon: string, title: string, description: string) : unit =
        send_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.System
            }
        )

    let Error (title: string, description: string) : unit =
        send_notification(
            {
                Icon = Icons.ALERT_CIRCLE
                Title = title
                Description = description
                Type = NotificationType.Error
            }
        )
