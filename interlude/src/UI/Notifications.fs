namespace Interlude.UI

open Percyqaz.Flux.UI
open Percyqaz.Flux.Windowing
open Prelude
open Interlude.UI

type Notification =
    private
        {
            Data: Callout
            Size: float32 * float32
            Fade: Animation.Fade
            FillColor: Color * Color
            ContentColor: Color * Color
            mutable Duration: float
        }

module Notifications =

    let displayed_notifications = ResizeArray<Notification>()

    let private add (body: Callout, colors: Color * Color, content_colors: Color * Color) : unit =
        GameThread.on_game_thread(fun () ->
            let n: Notification =
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
            let n: Notification =
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
        add(Callout.Small.Icon(icon).Title(title).Body(description), (Colors.pink_accent, Colors.pink), Colors.text)
        GameThread.on_game_thread Style.notify_task.Play

    let action_feedback (icon: string, title: string, description: string) : unit =
        add(Callout.Small.Icon(icon).Title(title).Body(description), (Colors.cyan_accent, Colors.cyan), Colors.text)
        GameThread.on_game_thread Style.notify_info.Play

    let action_feedback_button
        (icon: string, title: string, description: string, button_label: string, button_action: unit -> unit)
        : unit =
        add_long(
            Callout.Small.Icon(icon).Title(title).Body(description).Button(button_label, button_action),
            (Colors.cyan_accent, Colors.cyan),
            Colors.text
        )

        GameThread.on_game_thread Style.notify_info.Play

    let system_feedback (icon: string, title: string, description: string) : unit =
        add(Callout.Small.Icon(icon).Title(title).Body(description), (Colors.green_accent, Colors.green), Colors.text)
        GameThread.on_game_thread Style.notify_system.Play

    let error (title: string, description: string) : unit =
        add_long(
            Callout.Small.Icon(Icons.ALERT_CIRCLE).Title(title).Body(description),
            (Colors.red_accent, Colors.red),
            Colors.text
        )

        GameThread.on_game_thread Style.notify_error.Play
