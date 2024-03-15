namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Content.Noteskins
open Interlude.Utils
open Interlude.UI
open Interlude.Content
open Interlude.Features
open Interlude.Features.Play

type NoteskinPreview(scale: float32, rhs: bool) as this =
    inherit StaticContainer(NodeType.None)

    let fbo = FBO.create ()

    let create_renderer (info: Gameplay.Chart.LoadedChartInfo) =
        let playfield =
            Playfield(info.WithColors, PlayState.Dummy info, Content.NoteskinConfig, false)

        playfield.Add(LaneCover())

        if this.Initialised then
            playfield.Init this

        playfield :> Widget

    let mutable renderer: Widget = Dummy()

    let w = Viewport.vwidth * scale
    let h = Viewport.vheight * scale

    let bounds_placeholder =
        StaticContainer(
            NodeType.None,
            Position =
                if rhs then
                    {
                        Left = 1.0f %- (50.0f + w)
                        Top = 0.5f %- (h * 0.5f)
                        Right = 1.0f %- 50.0f
                        Bottom = 0.5f %+ (h * 0.5f)
                    }
                else
                    {
                        Left = 0.0f %+ 50.0f
                        Top = 0.5f %- (h * 0.5f)
                        Right = 0.0f %+ (50.0f + w)
                        Bottom = 0.5f %+ (h * 0.5f)
                    }
        )

    do
        fbo.Unbind()

        Gameplay.Chart.if_loaded <| fun info -> renderer <- create_renderer info

        this
        |* (bounds_placeholder
            |+ Text(
                Icons.EYE + " " + %"misc.preview",
                Align = Alignment.LEFT,
                Position = Position.Margin(20.0f, 10.0f).SliceTop(30.0f)
            ))

    member this.PreviewBounds = bounds_placeholder.Bounds

    member this.Refresh() =
        Gameplay.Chart.recolor ()
        Gameplay.Chart.when_loaded <| fun info -> renderer <- create_renderer info

    override this.Update(elapsed_ms, moved) =
        this.Bounds <- Viewport.bounds
        renderer.Update(elapsed_ms, moved)
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        fbo.Bind true
        Interlude.UI.Background.draw (Viewport.bounds, Colors.white, 1.0f)
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

[<AbstractClass>]
type ConfigPreview(scale: float32, config: Setting<WidgetPosition>) =
    inherit NoteskinPreview(scale, true)

    let keycount = int (Gameplay.Chart.keymode ())

    override this.Draw() =
        base.Draw()

        let container =
            if config.Value.Float then
                this.PreviewBounds
            else
                let cfg = Content.NoteskinConfig

                let width =
                    (cfg.ColumnWidth * float32 keycount
                     + Array.sum (cfg.KeymodeColumnSpacing keycount))
                    * scale

                let (screenAlign, columnAlign) = cfg.PlayfieldAlignment

                Rect
                    .Box(
                        this.PreviewBounds.Left + this.PreviewBounds.Width * screenAlign,
                        this.PreviewBounds.Top,
                        width,
                        this.PreviewBounds.Height
                    )
                    .Translate(-width * columnAlign, 0.0f)

        let width = container.Width
        let height = container.Height

        let leftA = config.Value.LeftA * width + container.Left
        let rightA = config.Value.RightA * width + container.Left
        let topA = config.Value.TopA * height + container.Top
        let bottomA = config.Value.BottomA * height + container.Top

        let bounds =
            Rect.Create(
                leftA + config.Value.Left * scale,
                topA + config.Value.Top * scale,
                rightA + config.Value.Right * scale,
                bottomA + config.Value.Bottom * scale
            )

        // Draw container
        Draw.rect
            (Rect
                .Create(container.Left, container.Top, container.Left, container.Bottom)
                .Expand(2.0f, 0.0f))
            Color.Red

        Draw.rect
            (Rect
                .Create(container.Right, container.Top, container.Right, container.Bottom)
                .Expand(2.0f, 0.0f))
            Color.Red

        Draw.rect
            (Rect
                .Create(container.Left, container.Top, container.Right, container.Top)
                .Expand(0.0f, 2.0f))
            Color.Red

        Draw.rect
            (Rect
                .Create(container.Left, container.Bottom, container.Right, container.Bottom)
                .Expand(0.0f, 2.0f))
            Color.Red
        // Draw alignments
        Draw.rect (Rect.Create(leftA, container.Top, leftA, container.Bottom).Expand(2.0f, 0.0f)) Color.Orange
        Draw.rect (Rect.Create(rightA, container.Top, rightA, container.Bottom).Expand(2.0f, 0.0f)) Color.Orange
        Draw.rect (Rect.Create(container.Left, topA, container.Right, topA).Expand(0.0f, 2.0f)) Color.Orange

        Draw.rect
            (Rect
                .Create(container.Left, bottomA, container.Right, bottomA)
                .Expand(0.0f, 2.0f))
            Color.Orange
        // Draw bounds
        Draw.rect
            (Rect
                .Create(bounds.Left, bounds.Top, bounds.Left, bounds.Bottom)
                .Expand(2.0f, 0.0f))
            Color.Lime

        Draw.rect
            (Rect
                .Create(bounds.Right, bounds.Top, bounds.Right, bounds.Bottom)
                .Expand(2.0f, 0.0f))
            Color.Lime

        Draw.rect
            (Rect
                .Create(bounds.Left, bounds.Top, bounds.Right, bounds.Top)
                .Expand(0.0f, 2.0f))
            Color.Lime

        Draw.rect
            (Rect
                .Create(bounds.Left, bounds.Bottom, bounds.Right, bounds.Bottom)
                .Expand(0.0f, 2.0f))
            Color.Lime

        this.DrawComponent(bounds)

    abstract member DrawComponent: Rect -> unit
