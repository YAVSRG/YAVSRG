﻿namespace Interlude.Features.Skins.EditHUD

open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.UI

[<AbstractClass>]
type ElementPreview(position: HudPosition) =
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
                        Render.height()
                    )
            else
                Render.bounds()
        let bounds = Position.calculate { Left = position.Left; Top = position.Top; Right = position.Right; Bottom = position.Bottom } container
        bounds.Width, bounds.Height

    override this.Draw() =

        let bounds = Rect.Box(this.Bounds.ShrinkL(PRETTYWIDTH).CenterX - width * 0.5f, this.Bounds.CenterY - height * 0.5f, width, height)
        // Draw bounds
        Render.rect
            (bounds.BorderL Style.PADDING)
            Color.Lime

        Render.rect
            (bounds.BorderCornersT Style.PADDING)
            Color.Lime

        Render.rect
            (bounds.BorderR Style.PADDING)
            Color.Lime

        Render.rect
            (bounds.BorderCornersB Style.PADDING)
            Color.Lime

        this.DrawComponent(bounds)

    abstract member DrawComponent: Rect -> unit