namespace Interlude.Features.Collections

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.UserDatabase
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI

type SelectCollectionsPage
    (on_select: string * Collection -> unit, is_disabled: string * Collection -> bool, select_on_create: bool) =
    inherit Page()

    let grid =
        GridFlowContainer<CollectionButton>(
            PAGE_ITEM_HEIGHT + 20.0f,
            2,
            Spacing = (20.0f, 20.0f),
            WrapNavigation = false
        )

    let refresh () =
        grid.Clear()

        for name, collection in Content.Collections.List do
            match collection with
            | Folder f ->
                grid.Add(
                    CollectionButton(
                        f.Icon.Value,
                        name,
                        (fun () -> on_select(name, collection)),
                        Disabled = is_disabled(name, collection)
                    )
                )
            | Playlist p ->
                grid.Add(
                    CollectionButton(
                        p.Icon.Value,
                        name,
                        (fun () -> on_select(name, collection)),
                        Disabled = is_disabled(name, collection)
                    )
                )

        if grid.Focused then
            grid.Focus(false)

    override this.Content() : Widget =
        refresh()

        page_container()
            .With(
                PageButton(
                    %"collections.create_folder",
                    (fun () -> CreateFolderPage(if select_on_create then on_select else ignore).Show())
                )
                    .Help(Help.Info("collections.create_folder"))
                    .Pos(0),
                PageButton(
                    %"collections.create_playlist",
                    (fun () -> CreatePlaylistPage("", if select_on_create then on_select else ignore).Show())
                )
                    .Help(Help.Info("collections.create_playlist"))
                    .Pos(2),
                ScrollContainer(grid).Pos(5, PAGE_BOTTOM - 5, PageWidth.Full)
            )

    override this.Title = sprintf "%s %s" Icons.FOLDER (%"collections")
    override this.OnReturnFromNestedPage() : unit = refresh()

    static member ManageCollections() : SelectCollectionsPage =
        SelectCollectionsPage(
            fun (name, collection) ->
                match collection with
                | Folder f -> EditFolderPage(name, f).Show()
                | Playlist p -> EditPlaylistPage(name, p).Show()
            , K false
            , false
        )

    static member AddToCollection(chart_meta: ChartMeta) : SelectCollectionsPage =
        SelectCollectionsPage(
            fun (name, collection) ->
                if CollectionActions.add_to(name, collection, chart_meta) then
                    Menu.Back()
            , fun (_, collection) ->
                match collection with
                | Folder f -> f.Contains(chart_meta)
                | Playlist _ -> false
            , true
        )
