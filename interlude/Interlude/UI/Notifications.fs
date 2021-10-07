namespace Interlude.UI

open System
open System.Drawing
open Interlude.Graphics
open Interlude
open Interlude.Input
open Interlude.UI.Animation

type NotificationType =
    | Info = 0
    | System = 1
    | Task = 2
    | Error = 3

module Notifications =

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
                    Text.drawFill(Themes.font(), s, r, Color.FromArgb(f, Color.White), 0.5f)
                    y <- y + notifHeight
                Stencil.finish()

    let display = Display()

    let add (str: string, t: NotificationType) =
        display.Parent.Value.Synchronized(
            fun () -> 
                let c =
                    match t with
                    | NotificationType.Info -> Color.Blue
                    | NotificationType.System -> Color.Green
                    | NotificationType.Task -> Color.Purple
                    | NotificationType.Error -> Color.Red
                    | _ -> Color.Black
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
    
    let mutable private active = false
    let mutable private bind = Dummy
    let mutable private text = [||]
    let mutable private timeLeft = 0.0
    let mutable private action = ignore
    let private fade = AnimationFade 0.0f

    type Display() as this =
        inherit Widget()

        let SCALE = 30.0f

        do
            this.Animation.Add fade

        override this.Update(elapsedTime, bounds) =
            if active then
                timeLeft <- timeLeft - elapsedTime
                if timeLeft <= 0.0 then (action(); active <- false; fade.Target <- 0.0f)
                elif bind.Released() then (active <- false; fade.Target <- 0.0f)
            base.Update(elapsedTime, bounds)

        override this.Draw() =
            if fade.Value > 0.01f then
                let x = Mouse.X()
                let mutable y = Mouse.Y() + 50.0f
                //todo: y-clamping
                for str in text do
                    let w = Text.measure(Themes.font(), str) * SCALE
                    //todo: x-clamping
                    Text.drawB(Themes.font(), str, SCALE, x - w * 0.5f, y, (Color.FromArgb(int(255.0f * fade.Value), Color.White), Color.FromArgb(int(255.0f * fade.Value), Color.Black)))
                    y <- y + SCALE
            base.Draw()

    let display = Display()

    let add (b: Bind, str: string, time: float, callback: unit -> unit) =
        if not active then
            active <- true
            fade.Target <- 1.0f
            bind <- b
            text <- str.Split "\n"
            timeLeft <- time
            action <- callback