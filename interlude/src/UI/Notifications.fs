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

    let private add_notification (notification: Notification) : unit =
        GameThread.on_game_thread(fun () -> notification_ev.Trigger(notification))

    let task_feedback (icon: string, title: string, description: string) : unit =
        add_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Task
            }
        )

    let action_feedback (icon: string, title: string, description: string) : unit =
        add_notification(
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
        add_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.Result(Some(button_label, button_action))
            }
        )

    let system_feedback (icon: string, title: string, description: string) : unit =
        add_notification(
            {
                Icon = icon
                Title = title
                Description = description
                Type = NotificationType.System
            }
        )

    let error (title: string, description: string) : unit =
        add_notification(
            {
                Icon = Icons.ALERT_CIRCLE
                Title = title
                Description = description
                Type = NotificationType.Error
            }
        )
