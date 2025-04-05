namespace Interlude.Features.Collections

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI

type private CreateFolderPage(on_create: (string * Collection) -> unit) =
    inherit Page()

    let new_name = Setting.simple "" |> Setting.alphanumeric
    let icon = Setting.simple Icons.HEART

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"collections.edit.folder_name", new_name).Pos(0)
        |+ PageSetting(%"collections.edit.icon", SelectDropdown(CreateFolderPage.Icons, icon))
            .Pos(3)
        |+ PageButton(
            %"confirm.yes",
            (fun () ->
                match Content.Collections.CreateFolder(new_name.Value, icon.Value) with
                | Some folder ->
                    Menu.Back()
                    on_create (new_name.Value, Folder folder)
                    CollectionActions.collection_modified_ev.Trigger()
                | None ->
                    Notifications.action_feedback (Icons.X, %"notification.collection_create_failed.title", %"notification.collection_create_failed.body")
            ),
            Disabled = fun () -> new_name.Value = ""
        )
            .Pos(6)
        :> Widget

    override this.Title = %"collections.create_folder"
    override this.OnClose() = ()

    static member Icons =
        [|
            Icons.HEART, Icons.HEART
            Icons.STAR, Icons.STAR
            Icons.BOOKMARK, Icons.BOOKMARK
            Icons.FOLDER, Icons.FOLDER
        |]

type CreatePlaylistPage(starting_name: string, on_create: (string * Collection) -> unit) =
    inherit Page()

    let new_name = Setting.simple starting_name |> Setting.alphanumeric
    let icon = Setting.simple Icons.HEART

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"collections.edit.playlist_name", new_name).Pos(0)
        |+ PageSetting(%"collections.edit.icon", SelectDropdown(CreatePlaylistPage.Icons, icon))
            .Pos(3)
        |+ PageButton(
            %"confirm.yes",
            (fun () ->
                match Content.Collections.CreatePlaylist(new_name.Value, icon.Value) with
                | Some playlist ->
                    Menu.Back()
                    on_create (new_name.Value, Playlist playlist)
                    CollectionActions.collection_modified_ev.Trigger()
                | None ->
                    Notifications.action_feedback (Icons.X, %"notification.collection_create_failed.title", %"notification.collection_create_failed.body")
            ),
            Disabled = fun () -> new_name.Value = ""
        )
            .Pos(6)
        :> Widget

    override this.Title = %"collections.create_playlist"
    override this.OnClose() = ()

    static member Icons =
        [|
            Icons.STAR, Icons.STAR
            Icons.FLAG, Icons.FLAG
            Icons.PLAY, Icons.PLAY
            Icons.LIST, Icons.LIST
        |]

type EditFolderPage(name: string, folder: Folder) =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphanumeric

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"collections.edit.folder_name", new_name).Pos(0)
        |+ PageSetting(%"collections.edit.icon", SelectDropdown(CreateFolderPage.Icons, folder.Icon))
            .Pos(2)
        |+ PageButton(
            %"collections.edit.delete",
            (fun () ->
                ConfirmPage(
                    [ name ] %> "misc.confirmdelete",
                    fun () ->
                        // todo: these operations should live in Actions.fs and collection_modified_ev should be private
                        if Content.Collections.Delete name then
                            CollectionActions.collection_modified_ev.Trigger()
                            Menu.Back()
                )
                    .Show()
            ),
            Icon = Icons.TRASH
        )
            .Pos(5)
        :> Widget

    override this.Title = name

    override this.OnClose() =
        if new_name.Value <> name && new_name.Value.Length > 1 then
            if Content.Collections.RenameCollection(name, new_name.Value) then
                CollectionActions.collection_modified_ev.Trigger()
            else
                Notifications.action_feedback (Icons.X, %"notification.collection_rename_failed.title", %"notification.collection_rename_failed.body")
                Logging.Debug "Rename failed, maybe that name already exists?"

type EditPlaylistPage(name: string, playlist: Playlist) =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphanumeric

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"collections.edit.playlist_name", new_name).Pos(0)
        |+ PageSetting(%"collections.edit.icon", SelectDropdown(CreatePlaylistPage.Icons, playlist.Icon))
            .Pos(2)
        |+ PageButton(
            %"collections.edit.delete",
            (fun () ->
                ConfirmPage(
                    [ name ] %> "misc.confirmdelete",
                    fun () ->
                        if Content.Collections.Delete name then
                            CollectionActions.collection_modified_ev.Trigger()
                            Menu.Back()
                )
                    .Show()
            ),
            Icon = Icons.TRASH
        )
            .Pos(5)
        :> Widget

    override this.Title = name

    override this.OnClose() =
        if new_name.Value <> name && new_name.Value.Length > 0 then
            if Content.Collections.RenamePlaylist(name, new_name.Value) then
                CollectionActions.collection_modified_ev.Trigger()
            else
                Notifications.action_feedback (Icons.X, %"notification.collection_rename_failed.title", %"notification.collection_rename_failed.body")
                Logging.Debug "Rename failed, maybe that name already exists?"

type private CollectionButton(icon: string, name: string, action: unit -> unit) as this =
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
            .With(
                Text(sprintf "%s %s" icon name)
                    .Align(Alignment.LEFT)
                    .Color(if this.Disabled then Colors.text_greyout else Colors.text)
                    .Position(Position.Shrink(20.0f, 15.0f))
            )
            .AddConditional(
                not this.Disabled,
                MouseListener().Button(this)
            )

        base.Init parent

    override this.OnFocus(by_mouse: bool) =
        base.OnFocus by_mouse
        Style.hover.Play()

    override this.Draw() =
        let color = if this.Focused then Colors.pink_accent else Colors.shadow_1
        let color = if this.Disabled then color.O3 else color

        Render.rect this.Bounds color.O3
        Render.rect (this.Bounds.Expand(0.0f, 5.0f).SliceB(5.0f)) color

        base.Draw()

type SelectCollectionPage
    (on_select: (string * Collection) -> unit, is_disabled: (string * Collection) -> bool, select_on_create: bool)
    =
    inherit Page()

    let grid =
        GridFlowContainer<CollectionButton>(PAGE_ITEM_HEIGHT + 20.0f, 2, Spacing = (20.0f, 20.0f), WrapNavigation = false)

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

    override this.Content() =
        refresh ()

        page_container()
        |+ PageButton(
            %"collections.create_folder",
            (fun () -> CreateFolderPage(if select_on_create then on_select else ignore).Show())
        )
            .Help(Help.Info("collections.create_folder"))
            .Pos(0)
        |+ PageButton(
            %"collections.create_playlist",
            (fun () -> CreatePlaylistPage("", if select_on_create then on_select else ignore).Show())
        )
            .Help(Help.Info("collections.create_playlist"))
            .Pos(2)
        |+ ScrollContainer(grid)
            .Pos(5, PAGE_BOTTOM - 5, PageWidth.Full)
        :> Widget

    override this.Title = sprintf "%s %s" Icons.FOLDER (%"collections")
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = refresh ()

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

type AddToCollectionPage(chart_meta: ChartMeta) =
    inherit
        SelectCollectionPage(
            fun (name, collection) ->
                if CollectionActions.add_to (name, collection, chart_meta) then
                    Menu.Back()
            , fun (_, collection) ->
                match collection with
                | Folder f -> f.Contains chart_meta
                | Playlist _ -> false
            , true
        )