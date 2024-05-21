namespace Interlude.Features.EditNoteskin

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Interlude.UI
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Play

type NoteskinPreview(position: Position) as this =
    inherit Container(NodeType.None)

    let fbo = FBO.create ()

    let create_renderer (info: LoadedChartInfo) =
        let playfield =
            Playfield(info.WithColors, PlayState.Dummy info, Content.NoteskinConfig, false)

        playfield.Add(LaneCover())

        if this.Initialised then
            playfield.Init this

        playfield :> Widget

    let mutable renderer: Widget = Dummy()

    let bounds_placeholder =
        Container(
            NodeType.None,
            Position = position
        )

    do
        fbo.Unbind()

        SelectedChart.if_loaded <| fun info -> renderer <- create_renderer info

        this
        |* (bounds_placeholder
            |+ Text(
                Icons.EYE + " " + %"misc.preview",
                Align = Alignment.LEFT,
                Position = Position.Margin(20.0f, 10.0f).SliceTop(30.0f)
            ))

    member this.PreviewBounds = bounds_placeholder.Bounds

    member this.Refresh() =
        SelectedChart.recolor ()
        SelectedChart.when_loaded <| fun info -> renderer <- create_renderer info

    override this.Update(elapsed_ms, moved) =
        this.Bounds <- Viewport.bounds
        renderer.Update(elapsed_ms, moved)
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        fbo.Bind true
        Background.draw (Viewport.bounds, Colors.white, 1.0f)
        Draw.rect Viewport.bounds (Color.Black.O4a(Interlude.Options.options.BackgroundDim.Value * 255.0f |> int))
        renderer.Draw()
        fbo.Unbind()
        Draw.rect (bounds_placeholder.Bounds.Translate(10.0f, 10.0f)) Colors.shadow_2.O2
        Draw.sprite bounds_placeholder.Bounds Color.White fbo.sprite
        base.Draw()

    override this.Init(parent: Widget) =
        base.Init parent
        this.Bounds <- Viewport.bounds
        renderer.Init this

    member this.Destroy() = fbo.Dispose()

    static member LEFT_HAND_SIDE (scale: float32) : Position =
        let w = Viewport.vwidth * scale
        let h = Viewport.vheight * scale
        {
            Left = 0.0f %+ 50.0f
            Top = 0.5f %- (h * 0.5f)
            Right = 0.0f %+ (50.0f + w)
            Bottom = 0.5f %+ (h * 0.5f)
        }
    static member RIGHT_HAND_SIDE (scale: float32) : Position =
        let w = Viewport.vwidth * scale
        let h = Viewport.vheight * scale
        {
            Left = 1.0f %- (50.0f + w)
            Top = 0.5f %- (h * 0.5f)
            Right = 1.0f %- 50.0f
            Bottom = 0.5f %+ (h * 0.5f)
        }