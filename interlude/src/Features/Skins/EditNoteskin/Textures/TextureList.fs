namespace Interlude.Features.Skins.EditNoteskin

open Percyqaz.Flux.UI
open Prelude.Skinning
open Prelude.Skinning.Noteskins
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

type TextureCard(noteskin: Noteskin, id: string, on_click: unit -> unit) as this =
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

    let mutable is_stitched = noteskin.TextureFileMode(id) = Grid

    do
        this
        |+ Image(sprite, Position = Position.TrimBottom(65.0f).Margin(20.0f), StretchToFill = false)
        |+ Text(id, Align = Alignment.CENTER, Position = Position.Margin(Style.PADDING).SliceBottom(40.0f).Translate(0.0f, -40.0f))
        |+ Clickable.Focus this
        |* Button(
            (fun () -> if is_stitched then Icons.SQUARE + " Grid" else Icons.GRID + " Loose"), 
            (fun () -> 
                (if is_stitched then noteskin.SplitTexture id else noteskin.StitchTexture id)
                is_stitched <- not is_stitched
            ),
            Position = Position.Margin(Style.PADDING).SliceBottom(40.0f)
        )

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

module TextureGrid =

    let create (noteskin: Noteskin) : Widget * (unit -> unit) =
        let textures_grid =
            GridFlowContainer<TextureCard>(
                40.0f + PRETTYWIDTH / 5f,
                5,
                WrapNavigation = false,
                Spacing = (15.0f, 15.0f)
            )

        let refresh () =
            textures_grid.Clear()

            for texture in noteskin.RequiredTextures do
                textures_grid |* TextureCard(noteskin, texture, (fun () -> TextureEditPage(texture).Show()))

        ScrollContainer(
            textures_grid,
            Margin = Style.PADDING,
            Position = pretty_pos(3, PAGE_BOTTOM - 3, PageWidth.Normal)
        ), refresh