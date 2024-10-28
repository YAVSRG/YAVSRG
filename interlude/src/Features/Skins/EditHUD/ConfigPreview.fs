namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Skins
open Interlude.UI

[<AbstractClass>]
type ConfigPreview(scale: float32, config: Setting<HudPosition>) =
    inherit SkinPreview(SkinPreview.RIGHT_HAND_SIDE scale)

    let keycount = int (SelectedChart.keymode ())

    override this.Draw() =
        base.Draw()

        let container =
            if config.Value.RelativeToPlayfield then
                let cfg = Content.NoteskinConfig

                let width =
                    (cfg.KeymodeColumnWidth keycount * float32 keycount
                     + Array.sum (cfg.KeymodeColumnSpacing keycount))
                    * scale

                let (screen_align, playfield_align) = cfg.PlayfieldAlignment

                Rect
                    .Box(
                        this.PreviewBounds.Left + this.PreviewBounds.Width * screen_align,
                        this.PreviewBounds.Top,
                        width,
                        this.PreviewBounds.Height
                    )
                    .Translate(-width * playfield_align, 0.0f)
            else
                this.PreviewBounds

        let width = container.Width
        let height = container.Height

        let leftA = snd config.Value.Left * width + container.Left
        let rightA = snd config.Value.Right * width + container.Left
        let topA = snd config.Value.Top * height + container.Top
        let bottomA = snd config.Value.Bottom * height + container.Top

        let bounds =
            Rect.Create(
                leftA + fst config.Value.Left * scale,
                topA + fst config.Value.Top * scale,
                rightA + fst config.Value.Right * scale,
                bottomA + fst config.Value.Bottom * scale
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

[<AbstractClass>]
type ConfigPreviewNew(position: HudPosition) =
    inherit StaticWidget(NodeType.None)

    let keycount = int (SelectedChart.keymode ())
    
    let width, height =
        let container =
            if position.RelativeToPlayfield then
                let cfg = Content.NoteskinConfig

                let width =
                    (cfg.KeymodeColumnWidth keycount * float32 keycount
                        + Array.sum (cfg.KeymodeColumnSpacing keycount))

                Rect
                    .Box(0.0f, 0.0f,
                        width,
                        Viewport.bounds.Height
                    )
            else
                Viewport.bounds
        let bounds = Position.calculate { Left = position.Left; Top = position.Top; Right = position.Right; Bottom = position.Bottom } container
        bounds.Width, bounds.Height

    override this.Draw() =
        
        let bounds = Rect.Box(this.Bounds.ShrinkL(PRETTYWIDTH).CenterX - width * 0.5f, this.Bounds.CenterY - height * 0.5f, width, height)
        // Draw bounds
        Draw.rect
            (bounds.BorderL Style.PADDING)
            Color.Lime

        Draw.rect
            (bounds.BorderCornersT Style.PADDING)
            Color.Lime

        Draw.rect
            (bounds.BorderR Style.PADDING)
            Color.Lime

        Draw.rect
            (bounds.BorderCornersB Style.PADDING)
            Color.Lime

        this.DrawComponent(bounds)

    abstract member DrawComponent: Rect -> unit