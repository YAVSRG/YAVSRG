namespace Percyqaz.Flux.Tests

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

type TopLevel() =
    inherit Root()

    let mutable time = 0.0

    let bind = Bind.mk OpenTK.Windowing.GraphicsLibraryFramework.Keys.A

    let setting = Setting.bounded 5f 0f 10f

    let fc = FlowContainer.Vertical(30.0f)

    let comp =
        for t in List.init 100 (sprintf "%s Hello %i" Percyqaz.Flux.Resources.Feather.award) do
            fc.Add(TextEntry(Setting.simple t, "none"))

        ScrollContainer.Flow(fc, Position = Position.Box(0.4f, 0.4f, 200.0f, 500.0f))

    let slider =
        Slider<float32>(setting, 0.1f, Position = Position.Box(0.3f, 0.8f, 300.0f, 60.0f))

    override this.Draw() =
        let (frames, ticks) = Render.FPS

        Text.draw (
            Style.baseFont,
            sprintf "FPS: %i in %.3f seconds" frames (float ticks / 10_000_000.0),
            30.0f,
            100.0f,
            100.0f,
            System.Drawing.Color.White
        )

        comp.Draw()
        slider.Draw()

        let x = 600.0f + 200.0f * (System.Math.Cos(time / 500.0) |> float32)
        Draw.rect (Rect.Box(x, 600.0f, 100.0f, 100.0f)) System.Drawing.Color.Yellow

        let cols =
            [ Palette.DARKER; Palette.DARK; Palette.BASE; Palette.LIGHT; Palette.LIGHTER ]

        let mutable r = Rect.Box(100.0f, 100.0f, 200.0f, 200.0f)

        for p, q in List.pairwise cols do
            Draw.rect r !*p
            Draw.rect (r.Shrink 10.0f) !*q
            r <- r.Translate(200.0f, 0.0f)

        let mutable r = Rect.Box(100.0f, 300.0f, 200.0f, 200.0f)

        for p, q in List.pairwise (List.rev cols) do
            Draw.rect r !*p
            Draw.rect (r.Shrink 10.0f) !*q
            r <- r.Translate(200.0f, 0.0f)

        let x, y = Mouse.pos ()
        Draw.rect (Rect.Box(x, y, 5.0f, 5.0f)) System.Drawing.Color.White

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        slider.Update(elapsedTime, moved)
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
        Style.changePrimaryColor (System.Drawing.Color.Maroon)
        comp.Init this
        slider.Init this
