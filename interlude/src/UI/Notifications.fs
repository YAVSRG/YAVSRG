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

    static member val OnNotificationAdded = notification_ev.Publish

    static member internal Send(notification: Notification) : unit =
        GameThread.on_game_thread(fun () -> notification_ev.Trigger(notification))

    static member internal Send(icon: string, title: string, description: string, ntype: NotificationType) : unit =
        Notifications.Send(
            {
                Icon = icon
                Title = title
                Description = description
                Type = ntype
            }
        )

    static member Task(icon: string, title: string, description: string) : unit =
        Notifications.Send(icon, title, description, NotificationType.Task)

    static member Task(icon: string, title: string) : unit = Notifications.Task(icon, title, "")

    static member Result(icon: string, title: string, description: string) : unit =
        Notifications.Send(icon, title, description, NotificationType.Result(None))

    static member Result(icon: string, title: string) : unit = Notifications.Result(icon, title, "")

    static member Result
        (icon: string, title: string, description: string, button_label: string, button_action: unit -> unit)
        : unit =
        Notifications.Send(icon, title, description, NotificationType.Result(Some(button_label, button_action)))

    static member System(icon: string, title: string, description: string) : unit =
        Notifications.Send(icon, title, description, NotificationType.System)

    static member System(icon: string, title: string) : unit = Notifications.System(icon, title, "")

    static member Error(title: string, description: string) : unit =
        Notifications.Send(Icons.ALERT_CIRCLE, title, description, NotificationType.Error)

    static member Error(title: string) : unit = Notifications.Error(title, "")
