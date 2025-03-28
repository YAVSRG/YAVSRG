namespace Interlude.Features.Skins

open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.UI

type TextureCard(source: Storage, id: string, on_click: unit -> unit) as this =
    inherit
        FrameContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                on_click ()
            ),
            Fill =
                (fun () ->
                    if this.Focused then
                        Colors.yellow_accent.O1
                    else
                        Colors.shadow_2.O2
                ),
            Border =
                (fun () ->
                    if this.Focused then
                        Colors.yellow_accent
                    else
                        Colors.grey_2.O3
                )
        )

    let sprite = Content.Texture id
    let mutable is_stitched = source.TextureIsGrid(id)

    override this.Init (parent: Widget) : unit =
        this.
            Add(
                Image(sprite, StretchToFill = false)
                    .Position(Position.ShrinkB(65.0f).Shrink(20.0f)),

                Text(id)
                    .Align(Alignment.CENTER)
                    .Position(Position.Shrink(Style.PADDING).SliceB(40.0f, 40.0f)),

                MouseListener().Button(this),

                Button(
                    (fun () -> if is_stitched then Icons.SQUARE + " " + %"skins.texture.grid" else Icons.GRID + " " + %"skins.texture.loose"),
                    (fun () ->
                        (if is_stitched then source.SplitTexture id else source.StitchTexture id)
                        is_stitched <- not is_stitched
                    )
                )
                    .Position(Position.Shrink(Style.PADDING).SliceB(40.0f))
        )

        base.Init(parent)

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

type TextureGrid =

    static member Noteskin(noteskin: Noteskin) : {| Container: ScrollContainer<GridFlowContainer<TextureCard>>; Refresh: unit -> unit |} =
        let textures_grid =
            GridFlowContainer<TextureCard>(40.0f + PAGE_ITEM_WIDTH / 5f, 5)
                .WrapNavigation(false)
                .Spacing(Style.PADDING * 3.0f)

        let refresh () =
            textures_grid.Clear()

            for texture in noteskin.RequiredTextures do
                textures_grid.Add(
                    TextureCard(noteskin, texture, fun () -> EditTexturePage(noteskin, texture).Show())
                )

        {|
            Container =
                ScrollContainer(textures_grid)
                    .Margin(Style.PADDING)
                    .Pos(3, PAGE_BOTTOM - 3, PageWidth.Normal)
            Refresh = refresh
        |}

    static member HUD(hud: HudLayout) : {| Container: ScrollContainer<GridFlowContainer<TextureCard>>; Refresh: unit -> unit |} =
        let textures_grid =
            GridFlowContainer<TextureCard>(40.0f + PAGE_ITEM_WIDTH / 5f, 5)
                .WrapNavigation(false)
                .Spacing(Style.PADDING * 3.0f)

        let refresh () =
            textures_grid.Clear()

            for texture in hud.RequiredTextures do
                textures_grid.Add(
                    TextureCard(hud, texture, fun () -> EditTexturePage(hud, texture).Show())
                )

        {|
            Container =
                ScrollContainer(textures_grid)
                    .Margin(Style.PADDING)
                    .Pos(0, PAGE_BOTTOM, PageWidth.Normal)
            Refresh = refresh
        |}