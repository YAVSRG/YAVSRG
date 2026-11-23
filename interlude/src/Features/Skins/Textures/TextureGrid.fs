namespace Interlude.Features.Skins

open Percyqaz.Flux.UI
open Prelude.Skins.Noteskins
open Prelude.Skins.HudLayouts
open Interlude.UI

type TextureGrid =

    static member Noteskin
        (noteskin: Noteskin)
        : {| Container: ScrollContainer<GridFlowContainer<TextureCard>>; Refresh: unit -> unit |} =
        let textures_grid =
            GridFlowContainer<TextureCard>(40.0f + PAGE_ITEM_WIDTH / 5f, 5)
                .WrapNavigation(false)
                .Spacing(Style.PADDING * 3.0f)

        let refresh () =
            textures_grid.Clear()

            for texture in noteskin.RequiredTextures do
                textures_grid.Add(TextureCard(noteskin, texture, fun () -> EditTexturePage(noteskin, texture).Show()))

        {|
            Container = ScrollContainer(textures_grid).Margin(Style.PADDING).Pos(3, PAGE_BOTTOM - 3, PageWidth.Normal)
            Refresh = refresh
        |}

    static member HUD
        (hud: HudLayout)
        : {| Container: ScrollContainer<GridFlowContainer<TextureCard>>; Refresh: unit -> unit |} =
        let textures_grid =
            GridFlowContainer<TextureCard>(40.0f + PAGE_ITEM_WIDTH / 5f, 5)
                .WrapNavigation(false)
                .Spacing(Style.PADDING * 3.0f)

        let refresh () =
            textures_grid.Clear()

            for texture in hud.RequiredTextures do
                textures_grid.Add(TextureCard(hud, texture, fun () -> EditTexturePage(hud, texture).Show()))

        {|
            Container = ScrollContainer(textures_grid).Margin(Style.PADDING).Pos(0, PAGE_BOTTOM, PageWidth.Normal)
            Refresh = refresh
        |}
