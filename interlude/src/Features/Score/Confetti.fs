namespace Interlude.Features.Score

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Interlude.UI

[<Struct>]
type private Confetto =
    {
        mutable X: float32
        mutable Y: float32
        XVel: float32
        mutable YVel: float32
        Color: int
    }

type Confetti () =
    inherit StaticWidget(NodeType.None)

    let colors = [|
        Colors.blue_accent; Colors.blue
        Colors.cyan_accent; Colors.cyan
        Colors.green_accent; Colors.green
        Colors.yellow_accent
        Colors.pink_accent; Colors.pink
        Colors.red_accent; Colors.red
    |]
    let mutable taken_screenshot_cooldown = 0.0

    let confetti : Confetto array = Array.zeroCreate 1000
    let mutable i = 0

    let random = System.Random()

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        taken_screenshot_cooldown <- taken_screenshot_cooldown - elapsed_ms
        if (%%"screenshot").Pressed() then
            if taken_screenshot_cooldown < 0.0 then
                Toolbar.take_screenshot ()
            else
                for _ = 1 to 100 do
                    let x, y = Mouse.pos()
                    confetti.[i] <-
                        {
                            X = x
                            Y = y
                            XVel = random.NextSingle() * 2.0f - 1.0f
                            YVel = -2.5f - random.NextSingle()
                            Color = random.Next(colors.Length)
                        }
                    i <- (i + 1) % confetti.Length
            taken_screenshot_cooldown <- 2000.0
        let dt = float32 elapsed_ms
        for i = 0 to confetti.Length - 1 do
            if confetti.[i].X > 0.0f && confetti.[i].Y < Render.height() then
                confetti.[i].X <- confetti.[i].X + confetti.[i].XVel * dt
                confetti.[i].Y <- confetti.[i].Y + confetti.[i].YVel * dt
                confetti.[i].YVel <- confetti.[i].YVel + 0.0075f * dt

    override this.Draw() =
        for i = 0 to confetti.Length - 1 do
            if confetti.[i].X > 0.0f && confetti.[i].Y > 0.0f && confetti.[i].Y < Render.height() then
                Render.rect (Rect.FromSize(confetti.[i].X, confetti.[i].Y, 12.0f, 12.0f)) colors.[confetti.[i].Color]