namespace Interlude.Features.EditNoteskin

open Percyqaz.Common
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Percyqaz.Flux.Input
open Prelude
open Prelude.Content
open Prelude.Content.Noteskins
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

type TextureCard(id: string, on_click: unit -> unit) as this =
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

    do
        this
        |+ Image(sprite, Position = Position.Margin(20.0f), StretchToFill = false)
        |+ Text(id, Align = Alignment.CENTER, Position = Position.Margin(Style.PADDING).SliceBottom(25.0f))
        |* Clickable.Focus this

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

module TextureGrid =

    let create (noteskin: Noteskin) : Widget * (unit -> unit) =
        let textures_grid =
            GridFlowContainer<TextureCard>(
                PRETTYWIDTH / 5f,
                5,
                WrapNavigation = false,
                Spacing = (15.0f, 15.0f),
                Position = pretty_pos(3, PAGE_BOTTOM - 3, PageWidth.Normal)
            )

        let refresh () =
            textures_grid.Clear()

            for texture in noteskin.RequiredTextures do
                textures_grid |* TextureCard(texture, (fun () -> TextureEditPage(texture).Show()))

        textures_grid, refresh