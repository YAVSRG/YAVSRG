namespace Interlude.Features.Toolbar

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Interlude.UI
open Interlude.Utils

type Updater() as this =
    inherit StaticContainer(NodeType.Button(fun () -> this.Click()))

    do this |* Clickable.Focus this

    override this.Draw() =
        Draw.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.shadow_2
        Draw.rect this.Bounds Colors.green_shadow

        if AutoUpdate.update_complete then
            Text.drawFillB(Style.font, Icons.reset + " Restart game", this.Bounds.Shrink(20.0f, 5.0f), (if this.Focused then Colors.text_yellow_2 else Colors.text_green_2), Alignment.CENTER)
        elif AutoUpdate.update_started then
            Text.drawFillB(Style.font, Icons.download + " Installing update..", this.Bounds.Shrink(20.0f, 5.0f), Colors.text_yellow_2, Alignment.CENTER)
        else 
            Text.drawFillB(Style.font, Icons.download + " Install update", this.Bounds.Shrink(20.0f, 5.0f), (if this.Focused then Colors.text_yellow_2 else Colors.text_green_2), Alignment.CENTER)

    member this.Click() =
        if AutoUpdate.update_complete then
            AutoUpdate.restart_on_exit <- true
            Screen.back Transitions.Flags.Default
        elif AutoUpdate.update_started then
            ()
        else 
            Notifications.system_feedback(Icons.system_notification, L"notification.update_installing.title", L"notification.update_installing.body")
            AutoUpdate.apply_update(fun () -> Notifications.system_feedback(Icons.system_notification, L"notification.update_installed.title", L"notification.update_installed.body"))
