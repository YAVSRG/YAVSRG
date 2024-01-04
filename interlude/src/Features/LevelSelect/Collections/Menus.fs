﻿namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Collections
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu

type private CreateFolderPage(on_create: (string * Collection) -> unit) as this =
    inherit Page()

    let new_name = Setting.simple "Folder" |> Setting.alphanumeric
    let icon = Setting.simple Icons.HEART

    do
        this.Content(
            column ()
            |+ PageTextEntry("collections.edit.folder_name", new_name).Pos(200.0f)
            |+ PageSetting("collections.edit.icon", Selector(CreateFolderPage.Icons, icon))
                .Pos(300.0f)
            |+ PageButton(
                "confirm.yes",
                (fun () ->
                    match collections.CreateFolder(new_name.Value, icon.Value) with
                    | Some folder ->
                        Menu.Back()
                        on_create (new_name.Value, Folder folder)
                    | None -> 
                        Notifications.action_feedback (Icons.X, "Name is taken", "A collection already exists with that name")
                )
            )
                .Pos(400.0f)
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
            column ()
            |+ PageTextEntry("collections.edit.playlist_name", new_name).Pos(200.0f)
            |+ PageSetting("collections.edit.icon", Selector(CreatePlaylistPage.Icons, icon))
                .Pos(300.0f)
            |+ PageButton(
                "confirm.yes",
                (fun () ->
                    match collections.CreatePlaylist(new_name.Value, icon.Value) with
                    | Some playlist ->
                        Menu.Back()
                        on_create (new_name.Value, Playlist playlist)
                    | None -> 
                        Notifications.action_feedback (Icons.X, "Name is taken", "A collection already exists with that name")
                )
            )
                .Pos(400.0f)
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

type private EditFolderPage(name: string, folder: Folder) as this =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphanumeric

    do
        let content =
            column ()
            |+ PageTextEntry("collections.edit.folder_name", new_name).Pos(200.0f)
            |+ PageSetting("collections.edit.icon", Selector(CreateFolderPage.Icons, folder.Icon))
                .Pos(270.0f)
            |+ PageButton(
                "collections.edit.delete",
                (fun () ->
                    ConfirmPage(
                        [ name ] %> "misc.confirmdelete",
                        fun () ->
                            if collections.Delete name then
                                if options.LibraryMode.Value = LibraryMode.Collections then
                                    LevelSelect.refresh_all ()

                                // todo: unselect collection when deleted

                                Menu.Back()
                    )
                        .Show()
                ),
                Icon = Icons.TRASH
            )
                .Pos(370.0f)

        this.Content content

    override this.Title = name

    override this.OnClose() =
        if new_name.Value <> name && new_name.Value.Length > 1 then
            if collections.RenameCollection(name, new_name.Value) then
                Logging.Debug(sprintf "Renamed collection '%s' to '%s'" name new_name.Value)
            else
                Notifications.action_feedback (Icons.X, "Rename failed", "A collection already exists with that name")
                Logging.Debug "Rename failed, maybe that name already exists?"

type private EditPlaylistPage(name: string, playlist: Playlist) as this =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphanumeric

    do
        let content =
            column ()
            |+ PageTextEntry("collections.edit.playlist_name", new_name).Pos(200.0f)
            |+ PageSetting("collections.edit.icon", Selector(CreatePlaylistPage.Icons, playlist.Icon))
                .Pos(270.0f)
            |+ PageButton(
                "collections.edit.delete",
                (fun () ->
                    ConfirmPage(
                        [ name ] %> "misc.confirmdelete",
                        fun () ->
                            if collections.Delete name then
                                if options.LibraryMode.Value = LibraryMode.Collections then
                                    LevelSelect.refresh_all ()

                                // todo: unselect collection when deleted

                                Menu.Back()
                    )
                        .Show()
                ),
                Icon = Icons.TRASH
            )
                .Pos(370.0f)

        this.Content content

    override this.Title = name

    override this.OnClose() =
        if new_name.Value <> name && new_name.Value.Length > 0 then
            if collections.RenamePlaylist(name, new_name.Value) then
                Logging.Debug(sprintf "Renamed playlist '%s' to '%s'" name new_name.Value)
            else
                Notifications.action_feedback (Icons.X, "Rename failed", "A collection already exists with that name")
                Logging.Debug "Rename failed, maybe that name already exists?"

type private CollectionButton(icon, name, action) as this =
    inherit
        StaticContainer(
            NodeType.Button(fun () ->
                Style.click.Play()
                if not this.Disabled then action ()
            )
        )

    member val Disabled = false with get, set

    override this.Init(parent: Widget) =
        this
        |+ Text(
            K(sprintf "%s %s" icon name),
            Align = Alignment.LEFT,
            Color = (if this.Disabled then K Colors.text_greyout else K Colors.text),
            Position = Position.Margin (20.0f, 15.0f)
        )
        |> fun x -> if not this.Disabled then x.Add <| Clickable.Focus this

        base.Init parent

    override this.OnFocus() =
        Style.hover.Play()
        base.OnFocus()

    override this.Draw() =
        let color =
            if this.Focused then Colors.pink_accent
            else Colors.shadow_1
        let color = if this.Disabled then color.O3 else color

        Draw.rect this.Bounds color.O3
        Draw.rect (this.Bounds.Expand(0.0f, 5.0f).SliceBottom(5.0f)) color

        base.Draw()

type SelectCollectionPage(on_select: (string * Collection) -> unit, is_disabled: (string * Collection) -> bool, select_on_create: bool) as this =
    inherit Page()

    let grid = GridFlowContainer<CollectionButton>(PRETTYHEIGHT + 20.0f, 2, Spacing = (20.0f, 20.0f))

    let refresh () =
        grid.Clear()

        for name, collection in collections.List do
            match collection with
            | Folder f -> 
                grid.Add(CollectionButton(f.Icon.Value, name, (fun () -> on_select (name, collection)), Disabled = is_disabled (name, collection)))
            | Playlist p ->
                grid.Add(CollectionButton(p.Icon.Value, name, (fun () -> on_select (name, collection)), Disabled = is_disabled (name, collection)))

        if grid.Focused then
            grid.Focus()

    do
        refresh ()
        this.Content(
            NavigationContainer.Column<Widget>()
            |+ PageButton("collections.create_folder", (fun () -> CreateFolderPage(if select_on_create then on_select else ignore).Show()))
                .Pos(150.0f)
                .Tooltip(Tooltip.Info("collections.create_folder"))
            |+ PageButton("collections.create_playlist", (fun () -> CreatePlaylistPage(if select_on_create then on_select else ignore).Show()))
                .Pos(220.0f)
                .Tooltip(Tooltip.Info("collections.create_playlist"))
            |+ ScrollContainer.Grid(grid, Position = Position.Margin(100.0f, 100.0f).TrimTop(280.0f))
        )

    override this.Title = %"collections.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh ()

    static member Editor() =
        SelectCollectionPage(
            fun (name, collection) ->
                match collection with
                | Folder f -> EditFolderPage(name, f).Show()
                | Playlist p -> EditPlaylistPage(name, p).Show()
            ,
            K false,
            false
        )
