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

module Notification =

    let private items = ResizeArray<Color * string * AnimationFade>()
    let private slider = new AnimationFade 0.0f

    let private notifWidth = 400.0f
    let private notifHeight = 35.0f

    type Display() as this =
        inherit Widget()

        do
            this.Animation.Add slider

        override this.Draw() =
            if items.Count > 0 then
                Stencil.create false
                Draw.rect this.Bounds Color.Transparent Sprite.Default
                Stencil.draw()
                let struct (_, _, _, b) = this.Bounds
                let m = Rect.centerX this.Bounds
                let mutable y = b - notifHeight * slider.Value
                for (c, s, f) in items do
                    let r = Rect.create (m - notifWidth) y (m + notifWidth) (y + notifHeight)
                    let f = f.Value * 255.0f |> int
                    Draw.rect r (Color.FromArgb(f / 2, c)) Sprite.Default
                    Text.drawFill(Content.font(), s, r, Color.FromArgb(f, Color.White), 0.5f)
                    y <- y + notifHeight
                Stencil.finish()

    let display = Display()

    let add (str: string, t: NotificationType) =
        display.Parent.Value.Synchronized(
            fun () -> 
                let c =
                    match t with
                    | Info -> Color.Blue
                    | Warning -> Color.Orange
                    | Error -> Color.Red
                    | System -> Color.Green
                    | Task -> Color.Purple
                slider.Target <- slider.Target + 1.0f
                let f = new AnimationFade((if items.Count = 0 then 0.0f else 1.0f), Target = 1.0f)
                display.Animation.Add f
                let i = (c, str, f)
                items.Add i
                display.Animation.Add(
                    Animation.Serial(
                        AnimationTimer 4000.0,
                        AnimationAction(fun () -> f.Target <- 0.0f),
                        AnimationTimer 1500.0,
                        AnimationAction(fun () -> slider.Target <- slider.Target - 1.0f; slider.Value <- slider.Value - 1.0f; f.Stop(); items.Remove i |> ignore)
                    )) )



module Tooltip =

    type private T =
        {
            Bind: Bind
            Message: string[]
            Type: NotificationType
            Callback: unit -> unit
            mutable Duration: float
            Fade: AnimationFade
        }

    let private items = ResizeArray<T>()

    let private HEIGHT = 120.0f
    let private TEXTHEIGHT = 42.0f

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
                    elif not (i.Bind.Pressed()) then
                        i.Fade.Target <- 0.0f
                elif i.Fade.Value < 0.01f then this.Synchronized(fun () -> items.Remove i |> ignore)
            base.Update(elapsedTime, bounds)

        override this.Draw() =
            let struct (left, top, right, bottom) = this.Bounds
            let mutable y = bottom - 200.0f
            for i in items do
                let h = HEIGHT + TEXTHEIGHT * float32 (i.Message.Length - 1)
                let a = i.Fade.Value * 255.0f |> int
                y <- y - h * i.Fade.Value
                let bounds = Rect.create (left + 100.0f) y (right - 100.0f) (y + h)
                let c, icon =
                    match i.Type with
                    | Info -> Color.FromArgb(0, 150, 180), "ⓘ"
                    | Warning -> Color.Orange, "⚠"
                    | Error -> Color.Red, "⚠"
                    | System -> Color.Green, "❖"
                    | Task -> Color.Purple, "❖"
                Draw.rect (Rect.sliceTop 5.0f bounds) (Color.FromArgb(a, c)) Sprite.Default
                Draw.rect (Rect.sliceBottom 5.0f bounds) (Color.FromArgb(a, c)) Sprite.Default
                Draw.rect (Rect.sliceLeft 5.0f bounds) (Color.FromArgb(a, c)) Sprite.Default
                Draw.rect (Rect.sliceRight 5.0f bounds) (Color.FromArgb(a, c)) Sprite.Default
                
                Draw.rect (Rect.expand (-5.0f, -5.0f) bounds) (Color.FromArgb(a / 4 * 3, Color.Black)) Sprite.Default
                Draw.rect (Rect.expand (-5.0f, -5.0f) bounds) (Color.FromArgb(a / 2, c)) Sprite.Default
                
                Text.drawB (Content.font(), icon, 50.0f, left + 130.0f, y - 1.0f + TEXTHEIGHT * 0.5f * float32 i.Message.Length, (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black)))
                for x = 0 to i.Message.Length - 1 do
                    Text.drawB (Content.font(), i.Message.[x], 30.0f, left + 235.0f, y + 33.0f + TEXTHEIGHT * float32 x, (Color.FromArgb(a, Color.White), Color.FromArgb(a, Color.Black)))

            base.Draw()

    let display = Display()

    let add (b: Bind, str: string, time: float) =
        let t: T =
            {
                Bind = b
                Message = str.Split "\n"
                Duration = time
                Fade = AnimationFade(0.0f, Target = 1.0f)
                Callback = ignore
                Type = Info
            }
        display.Synchronized(fun () -> items.Add t)