namespace Interlude.Features.OptionsMenu.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Charts.Tools.NoteColors
open Prelude.Data.Content
open Interlude.Content
open Interlude.Features
open Interlude.Features.Play

type NoteskinPreview(scale: float32) as this =
    inherit StaticContainer(NodeType.None)

    let fbo = FBO.create ()

    let create_renderer (with_colors: ColoredChart) =
        let playfield =
            Playfield(with_colors, PlayState.Dummy with_colors.Source, noteskin_config (), false)

        playfield.Add(LaneCover())
            
        if this.Initialised then
            playfield.Init this

        playfield :> Widget

    let mutable renderer : Widget = Dummy()

    let w = Viewport.vwidth * scale
    let h = Viewport.vheight * scale

    let bounds_placeholder =
        StaticContainer(
            NodeType.None,
            Position =
                {
                    Left = 1.0f %- (50.0f + w)
                    Top = 0.5f %- (h * 0.5f)
                    Right = 1.0f %- 50.0f
                    Bottom = 0.5f %+ (h * 0.5f)
                }
        )

    do
        fbo.Unbind()

        Gameplay.Chart.if_loaded <| fun info -> 
            renderer <- create_renderer info.WithColors

        this
        |* (bounds_placeholder
            |+ Text(
                "PREVIEW",
                Position =
                    { Position.Default with
                        Top = 1.0f %+ 0.0f
                        Bottom = 1.0f %+ 50.0f
                    }
            ))

    member this.PreviewBounds = bounds_placeholder.Bounds

    member this.Refresh() =
        if Gameplay.Chart.CHART.IsSome then
            Gameplay.Chart.recolor ()
            Gameplay.Chart.when_loaded <| fun info -> renderer <- create_renderer info.WithColors

    override this.Update(elapsed_ms, moved) =
        this.Bounds <- Viewport.bounds
        renderer.Update(elapsed_ms, moved)
        base.Update(elapsed_ms, moved)

    override this.Draw() =
        fbo.Bind true
        renderer.Draw()
        fbo.Unbind()
        Draw.sprite bounds_placeholder.Bounds Color.White fbo.sprite
        base.Draw()

    override this.Init(parent: Widget) =
        base.Init parent
        this.Bounds <- Viewport.bounds
        renderer.Init this

    member this.Destroy() = fbo.Dispose()

[<AbstractClass>]
type ConfigPreview(scale: float32, config: Setting<WidgetPosition>) =
    inherit NoteskinPreview(scale)

    let keycount = match Gameplay.Chart.CACHE_DATA with Some cc -> cc.Keys | None -> 4

    override this.Draw() =
        base.Draw()

        let container =
            if config.Value.Float then
                this.PreviewBounds
            else
                let cfg = Noteskins.Current.config

                let width =
                    cfg.ColumnWidth
                    * float32 keycount
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
