namespace Interlude.UI

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Percyqaz.Flux.Windowing
open Interlude.UI

[<Sealed>]
type NotificationsDisplay() =
    inherit Overlay(NodeType.None)

    override this.Update(elapsed_ms: float, moved: bool) : unit =
        base.Update(elapsed_ms, moved)

        let mutable y = this.Bounds.Top + 80.0f

        for i in Notifications.displayed_notifications do
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
                GameThread.defer(fun () -> Notifications.displayed_notifications.Remove(i) |> ignore)

    override this.Draw() : unit =
        let mutable y = this.Bounds.Top + 80.0f

        for i in Notifications.displayed_notifications do
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
