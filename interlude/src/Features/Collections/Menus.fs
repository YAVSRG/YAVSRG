namespace Interlude.Features.Collections

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu

type private CreateFolderPage(on_create: (string * Collection) -> unit) as this =
    inherit Page()

    let new_name = Setting.simple "Folder" |> Setting.alphanumeric
    let icon = Setting.simple Icons.HEART

    do
        this.Content(
            page_container()
            |+ PageTextEntry("collections.edit.folder_name", new_name).Pos(0)
            |+ PageSetting("collections.edit.icon", Selector(CreateFolderPage.Icons, icon))
                .Pos(3)
            |+ PageButton(
                "confirm.yes",
                (fun () ->
                    match Content.Collections.CreateFolder(new_name.Value, icon.Value) with
                    | Some folder ->
                        Menu.Back()
                        on_create (new_name.Value, Folder folder)
                    | None ->
                        Notifications.action_feedback (
                            Icons.X,
                            "Name is taken",
                            "A collection already exists with that name"
                        )
                )
            )
                .Pos(6)
        )

    override this.Title = %"collections.create_folder.name"
    override this.OnClose() = ()

    static member Icons =
        [|
            Icons.HEART, Icons.HEART
            Icons.STAR, Icons.STAR
            Icons.BOOKMARK, Icons.BOOKMARK
            Icons.FOLDER, Icons.FOLDER
        |]

type private CreatePlaylistPage(on_create: (string * Collection) -> unit) as this =
    inherit Page()

    let new_name = Setting.simple "Playlist" |> Setting.alphanumeric
    let icon = Setting.simple Icons.HEART

    do
        this.Content(
            page_container()
            |+ PageTextEntry("collections.edit.playlist_name", new_name).Pos(0)
            |+ PageSetting("collections.edit.icon", Selector(CreatePlaylistPage.Icons, icon))
                .Pos(3)
            |+ PageButton(
                "confirm.yes",
                (fun () ->
                    match Content.Collections.CreatePlaylist(new_name.Value, icon.Value) with
                    | Some playlist ->
                        Menu.Back()
                        on_create (new_name.Value, Playlist playlist)
                    | None ->
                        Notifications.action_feedback (
                            Icons.X,
                            "Name is taken",
                            "A collection already exists with that name"
                        )
                )
            )
                .Pos(6)
        )

    override this.Title = %"collections.create_playlist.name"
    override this.OnClose() = ()

    static member Icons =
        [|
            Icons.STAR, Icons.STAR
            Icons.FLAG, Icons.FLAG
            Icons.PLAY, Icons.PLAY
            Icons.LIST, Icons.LIST
        |]

type EditFolderPage(name: string, folder: Folder) as this =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphanumeric

    do
        let content =
            page_container()
            |+ PageTextEntry("collections.edit.folder_name", new_name).Pos(0)
            |+ PageSetting("collections.edit.icon", Selector(CreateFolderPage.Icons, folder.Icon))
                .Pos(2)
            |+ PageButton(
                "collections.edit.delete",
                (fun () ->
                    ConfirmPage(
                        [ name ] %> "misc.confirmdelete",
                        fun () ->
                            if Content.Collections.Delete name then
                                CollectionActions.collection_modified_ev.Trigger()

                                // todo: unselect collection when deleted

                                Menu.Back()
                    )
                        .Show()
                ),
                Icon = Icons.TRASH
            )
                .Pos(5)

        this.Content content

    override this.Title = name

    override this.OnClose() =
        if new_name.Value <> name && new_name.Value.Length > 1 then
            if Content.Collections.RenameCollection(name, new_name.Value) then
                Logging.Debug(sprintf "Renamed collection '%s' to '%s'" name new_name.Value)
                CollectionActions.collection_modified_ev.Trigger()
            else
                Notifications.action_feedback (Icons.X, "Rename failed", "A collection already exists with that name")
                Logging.Debug "Rename failed, maybe that name already exists?"

type EditPlaylistPage(name: string, playlist: Playlist) as this =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphanumeric

    do
        let content =
            page_container()
            |+ PageTextEntry("collections.edit.playlist_name", new_name).Pos(0)
            |+ PageSetting("collections.edit.icon", Selector(CreatePlaylistPage.Icons, playlist.Icon))
                .Pos(2)
            |+ PageButton(
                "collections.edit.delete",
                (fun () ->
                    ConfirmPage(
                        [ name ] %> "misc.confirmdelete",
                        fun () ->
                            if Content.Collections.Delete name then
                                CollectionActions.collection_modified_ev.Trigger()

                                // todo: unselect collection when deleted

                                Menu.Back()
                    )
                        .Show()
                ),
                Icon = Icons.TRASH
            )
                .Pos(5)

        this.Content content

    override this.Title = name

    override this.OnClose() =
        if new_name.Value <> name && new_name.Value.Length > 0 then
            if Content.Collections.RenamePlaylist(name, new_name.Value) then
                Logging.Debug(sprintf "Renamed playlist '%s' to '%s'" name new_name.Value)
                CollectionActions.collection_modified_ev.Trigger()
            else
                Notifications.action_feedback (Icons.X, "Rename failed", "A collection already exists with that name")
                Logging.Debug "Rename failed, maybe that name already exists?"

type private CollectionButton(icon, name, action) as this =
    inherit
        Container(
            NodeType.Button(fun () ->
                Style.click.Play()

                if not this.Disabled then
                    action ()
            )
        )

    member val Disabled = false with get, set

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(sprintf "%s %s" icon name),
            Align = Alignment.LEFT,
            Color =
                (if this.Disabled then
                     K Colors.text_greyout
                 else
                     K Colors.text),
            Position = Position.Margin(20.0f, 15.0f)
        )
        |> fun x ->
            if not this.Disabled then
                x.Add <| Clickable.Focus this

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let color = if this.Focused then Colors.pink_accent else Colors.shadow_1
        let color = if this.Disabled then color.O3 else color

        Draw.rect this.Bounds color.O3
        Draw.rect (this.Bounds.Expand(0.0f, 5.0f).SliceBottom(5.0f)) color

        base.Draw()

type SelectCollectionPage
    (on_select: (string * Collection) -> unit, is_disabled: (string * Collection) -> bool, select_on_create: bool) as this
    =
    inherit Page()

    let grid =
        GridFlowContainer<CollectionButton>(PRETTYHEIGHT + 20.0f, 2, Spacing = (20.0f, 20.0f), WrapNavigation = false)

    let refresh () =
        grid.Clear()

        for name, collection in Content.Collections.List do
            match collection with
            | Folder f ->
                grid.Add(
                    CollectionButton(
                        f.Icon.Value,
                        name,
                        (fun () -> on_select (name, collection)),
                        Disabled = is_disabled (name, collection)
                    )
                )
            | Playlist p ->
                grid.Add(
                    CollectionButton(
                        p.Icon.Value,
                        name,
                        (fun () -> on_select (name, collection)),
                        Disabled = is_disabled (name, collection)
                    )
                )

        if grid.Focused then
            grid.Focus false

    do
        refresh ()

        this.Content(
            page_container()
            |+ PageButton(
                "collections.create_folder",
                (fun () -> CreateFolderPage(if select_on_create then on_select else ignore).Show())
            )
                .Pos(0)
                .Tooltip(Tooltip.Info("collections.create_folder"))
            |+ PageButton(
                "collections.create_playlist",
                (fun () -> CreatePlaylistPage(if select_on_create then on_select else ignore).Show())
            )
                .Pos(2)
                .Tooltip(Tooltip.Info("collections.create_playlist"))
            |+ ScrollContainer(grid, Position = pretty_pos(5, PAGE_BOTTOM - 5, PageWidth.Full))
        )

    override this.Title = %"collections.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh ()

type ManageCollectionsPage() =
    inherit
        SelectCollectionPage(
            fun (name, collection) ->
                match collection with
                | Folder f -> EditFolderPage(name, f).Show()
                | Playlist p -> EditPlaylistPage(name, p).Show()
            , K false
            , false
        )

type AddToCollectionPage(cc: CachedChart) =
    inherit
        SelectCollectionPage(
            fun (name, collection) ->
                if CollectionActions.add_to (name, collection, cc) then
                    Menu.Back()
            , fun (_, collection) ->
                match collection with
                | Folder f -> f.Contains cc
                | Playlist _ -> false
            , true
        )
