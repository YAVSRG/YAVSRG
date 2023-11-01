namespace Interlude.Features.Toolbar

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.UI
open Interlude.Utils

type Updater() as this =
    inherit StaticContainer(NodeType.Button(fun () -> this.Click()))

    do this |* Clickable.Focus this

    override this.Draw() =
        let area = this.Bounds.TrimBottom(15.0f)
        Draw.rect area (Colors.shadow_1.O2)

        if AutoUpdate.update_complete then
            Text.fill_b (
                Style.font,
                Icons.reset + " Restart game",
                area.Shrink(20.0f, 5.0f),
                (if this.Focused then
                     Colors.text_yellow_2
                 else
                     Colors.text_green_2),
                Alignment.CENTER
            )
        elif AutoUpdate.update_started then
            Text.fill_b (
                Style.font,
                Icons.download + " Installing update..",
                area.Shrink(20.0f, 5.0f),
                Colors.text_yellow_2,
                Alignment.CENTER
            )
        else
            Text.fill_b (
                Style.font,
                Icons.download + " Install update",
                area.Shrink(20.0f, 5.0f),
                (if this.Focused then
                     Colors.text_yellow_2
                 else
                     Colors.text_green_2),
                Alignment.CENTER
            )

    member this.Click() =
        if AutoUpdate.update_complete then
            AutoUpdate.restart_on_exit <- true
            Screen.change Screen.Type.SplashScreen Transitions.Flags.UnderLogo
        elif AutoUpdate.update_started then
            ()
        else
            Notifications.system_feedback (
                Icons.system_notification,
                %"notification.update_installing.title",
                %"notification.update_installing.body"
            )

            AutoUpdate.apply_update (fun () ->
                Notifications.system_feedback (
                    Icons.system_notification,
                    %"notification.update_installed.title",
                    %"notification.update_installed.body"
                )
            )
