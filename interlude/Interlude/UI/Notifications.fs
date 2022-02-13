namespace Interlude.UI

open System
open System.Drawing
open Interlude.Graphics
open Interlude
open Interlude.Input
open Interlude.UI.Animation

type NotificationType =
    | Info
    | Warning
    | Error
    | System
    | Task

module Tooltip =

    type private T =
        {
            Bind: Bind option
            Message: string[]
            Type: NotificationType
            Callback: unit -> unit
            mutable Duration: float
            Fade: AnimationFade
        }

    let private items = ResizeArray<T>()

    let private HEIGHT = 120.0f
    let private TEXTHEIGHT = 42.0f
    let mutable up = false

    type Display() =
        inherit Widget()

        override this.Update(elapsedTime, bounds) =
            for i in items do
                i.Duration <- i.Duration - elapsedTime
                i.Fade.Update elapsedTime |> ignore
                if i.Fade.Target <> 0.0f then
                    if i.Duration <= 0.0 then
                        i.Fade.Target <- 0.0f
                        i.Callback()
                    elif i.Bind.IsSome && not (i.Bind.Value.Pressed()) then
                        i.Fade.Target <- 0.0f
                elif i.Fade.Value < 0.01f then this.Synchronized(fun () -> items.Remove i |> ignore)
            base.Update(elapsedTime, bounds)
            if items.Count = 0 then
                up <- Mouse.Y() > Rect.centerY this.Bounds

        override this.Draw() =
            let struct (left, top, right, bottom) = this.Bounds
            let height i = HEIGHT + TEXTHEIGHT * float32 (i.Message.Length - 1)
            let draw i y h =
                let bounds = Rect.create (left + 100.0f) y (right - 100.0f) (y + h)
                let c, icon =
                    match i.Type with
                    | Info -> Color.FromArgb(0, 120, 190), Icons.info
                    | Warning -> Color.FromArgb(180, 150, 0), Icons.alert
                    | Error -> Color.FromArgb(190, 0, 0), Icons.alert
                    | System -> Color.FromArgb(0, 190, 120), Icons.system_notification
                    | Task -> Color.FromArgb(120, 0, 190), Icons.system_notification
                let a = i.Fade.Value * 255.0f |> int
                Draw.rect (Rect.sliceTop 5.0f bounds) (Color.FromArgb(a, c)) Sprite.Default
                Draw.rect (Rect.sliceBottom 5.0f bounds) (Color.FromArgb(a, c)) Sprite.Default
                Draw.rect (Rect.sliceLeft 5.0f bounds) (Color.FromArgb(a, c)) Sprite.Default
                Draw.rect (Rect.sliceRight 5.0f bounds) (Color.FromArgb(a, c)) Sprite.Default
                
                Draw.rect (Rect.expand (-5.0f, -5.0f) bounds) (Color.FromArgb(a / 4 * 3, Color.Black)) Sprite.Default
                Draw.rect (Rect.expand (-5.0f, -5.0f) bounds) (Color.FromArgb(a / 2, c)) Sprite.Default
                
                Text.drawB (Content.font, icon, 50.0f, left + 130.0f, y - 1.0f + TEXTHEIGHT * 0.5f * float32 i.Message.Length, (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black)))
                for x = 0 to i.Message.Length - 1 do
                    Text.drawB (Content.font, i.Message.[x], 30.0f, left + 235.0f, y + 33.0f + TEXTHEIGHT * float32 x, (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black)))

            if up then
                let mutable y = top + 200.0f
                for i in items do
                    let h = height i
                    y <- y + h * i.Fade.Value
                    draw i (y - h) h
            else
                let mutable y = bottom - 200.0f
                for i in items do
                    let h = height i
                    y <- y - h * i.Fade.Value
                    draw i y h
            base.Draw()

    let display = Display()

    let tooltip (b: Bind, str: string) =
        let t: T =
            {
                Bind = Some b
                Message = str.Split "\n"
                Duration = infinity
                Fade = AnimationFade(0.0f, Target = 1.0f)
                Callback = ignore
                Type = Info
            }
        display.Synchronized(fun () -> items.Add t)

    let notif (str: string, t: NotificationType) =
        let t: T =
            {
                Bind = None
                Message = str.Split "\n"
                Duration = 2000.0
                Fade = AnimationFade(0.0f, Target = 1.0f)
                Callback = ignore
                Type = t
            }
        display.Synchronized(fun () -> items.Add t)

    let callback (b: Bind, str: string, t: NotificationType, cb: unit -> unit) =
        let t: T =
            {
                Bind = Some b
                Message = str.Split "\n"
                Duration = 2000.0
                Fade = AnimationFade(0.0f, Target = 1.0f)
                Callback = cb
                Type = t
            }
        display.Synchronized(fun () -> items.Add t)

module Notification =

    let add (str: string, t: NotificationType) =
        Tooltip.notif (str, t)