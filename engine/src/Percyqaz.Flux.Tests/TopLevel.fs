namespace Percyqaz.Flux.Tests

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

type TopLevel() =
    inherit Root()

    let mutable time = 0.0

    let bind = Bind.mk OpenTK.Windowing.GraphicsLibraryFramework.Keys.A

    let fc = FlowContainer.Vertical(30.0f)
    let comp = 
        for t in List.init 100 (sprintf "%s Hello %i" Percyqaz.Flux.Resources.Feather.award) do
            fc.Add( TextEntry (Setting.simple t, "none") )
        ScrollContainer.Flow(fc, Position = Position.Box(0.4f, 0.4f, 200.0f, 500.0f))

    override this.Draw() =
        Text.draw(Style.baseFont, "Hello world", 30.0f, 100.0f, 100.0f, System.Drawing.Color.White)
        comp.Draw()

        let x = 600.0f + 200.0f * (System.Math.Cos(time / 500.0) |> float32)
        Draw.rect (Rect.Box(x, 600.0f, 100.0f, 100.0f)) System.Drawing.Color.Yellow

        let x, y = Mouse.pos()
        Draw.rect (Rect.Box(x, y, 5.0f, 5.0f)) System.Drawing.Color.White

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        comp.Update(elapsedTime, moved)
        time <- time + elapsedTime
        if bind.Tapped() then
            fc.Spacing <- System.Random().NextDouble() * 20.0 |> float32
            time <- time + 5000.0
            comp.Focus()

    override this.Init() =
        base.Init()
        Style.baseFont <- Fonts.create "Inconsolata"
        Style.baseFont.SpaceWidth <- Style.baseFont.SpaceWidth * 2.0f
        comp.Init this