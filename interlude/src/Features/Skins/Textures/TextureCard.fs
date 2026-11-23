namespace Interlude.Features.Skins

open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins
open Interlude.Content
open Interlude.UI

type TextureCard(source: Storage, id: string, on_click: unit -> unit) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click()
            ),
            Fill = (fun () -> if this.Focused then Colors.yellow_accent.O1 else Colors.shadow_2.O2),
            Border = (fun () -> if this.Focused then Colors.yellow_accent else Colors.grey_2.O3)
        )

    let sprite = Content.Texture(id)
    let mutable is_stitched = source.TextureIsGrid(id)

    override this.Init(parent: Widget) : unit =
        this.Add(
            Image(sprite, StretchToFill = false).Position(Position.ShrinkB(65.0f).Shrink(20.0f)),
            Text(id).Align(Alignment.CENTER).Position(Position.Shrink(Style.PADDING).SliceB(40.0f, 40.0f)),
            MouseListener().Button(this),
            Button(
                (fun () ->
                    if is_stitched then
                        Icons.SQUARE + " " + %"skins.texture.grid"
                    else
                        Icons.GRID + " " + %"skins.texture.loose"
                ),
                (fun () ->
                    (if is_stitched then source.SplitTexture(id) else source.StitchTexture(id))
                    is_stitched <- not is_stitched
                )
            )
                .Position(Position.Shrink(Style.PADDING).SliceB(40.0f))
        )

        base.Init(parent)

    override this.OnFocus(by_mouse: bool) : unit =
        base.OnFocus(by_mouse)
        Style.hover.Play()
