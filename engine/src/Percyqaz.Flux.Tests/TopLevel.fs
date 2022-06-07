namespace Percyqaz.Flux.Tests

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input

type TopLevel() =
    inherit Root()

    let bind = Bind.mk OpenTK.Windowing.GraphicsLibraryFramework.Keys.A

    let fc = FlowContainer.Vertical(30.0f)
    let comp = 
        for t in List.init 100 (sprintf "%s Hello %i!!" Percyqaz.Flux.Resources.Feather.activity) do
            fc.Add( Text t )
        ScrollContainer.Flow(fc, Position = Position.Box(0.4f, 0.4f, 200.0f, 500.0f))

    override this.Draw() =
        Text.draw(Style.baseFont, "Hello world", 30.0f, 100.0f, 100.0f, System.Drawing.Color.White)
        comp.Draw()
        let x, y = Mouse.pos()
        Draw.rect (Rect.Box(x, y, 5.0f, 5.0f)) System.Drawing.Color.White Sprite.Default 

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        comp.Update(elapsedTime, moved)
        if bind.Tapped() then fc.Spacing <- System.Random().NextDouble() * 20.0 |> float32

    override this.Init() =
        base.Init()
        Style.baseFont <- Fonts.create "Courier Prime Sans"
        Style.baseFont.SpaceWidth <- Style.baseFont.SpaceWidth * 2.0f
        comp.Init this