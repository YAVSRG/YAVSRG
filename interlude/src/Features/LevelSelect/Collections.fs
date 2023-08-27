namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Collections
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features.Gameplay

module CollectionManager =

    let add_to (name: string, collection: Collection, cc: CachedChart) =
        if
            match collection with
            | Folder c -> c.Add cc
            | Playlist p -> p.Add (cc, rate.Value, selectedMods.Value)
        then
            if options.LibraryMode.Value = LibraryMode.Collections then LevelSelect.refresh_all() else LevelSelect.refresh_details()
            Notifications.action_feedback (Icons.add_to_collection, Localisation.localiseWith [cc.Title; name] "collections.added", "")
            true
        else false

    let remove_from (name: string, collection: Collection, cc: CachedChart, context: LibraryContext) =
        if
            match collection with
            | Folder c -> c.Remove cc
            | Playlist p ->
                match context with
                | LibraryContext.Playlist (i, in_name, _) when name = in_name -> p.RemoveAt i
                | _ -> p.RemoveSingle cc
        then
            if options.LibraryMode.Value <> LibraryMode.All then LevelSelect.refresh_all() else LevelSelect.refresh_details()
            Notifications.action_feedback (Icons.remove_from_collection, Localisation.localiseWith [cc.Title; name] "collections.removed", "")
            if Some cc = Chart.cacheInfo then Chart.context <- LibraryContext.None
            true
        else false

    module Current =

        let quick_add(cc: CachedChart) =
            match options.SelectedCollection.Value with
            | Some coll -> add_to (coll, Collections.current.Value, cc)
            | None -> false

        let quick_remove(cc: CachedChart, context: LibraryContext) =
            match options.SelectedCollection.Value with
            | Some coll -> remove_from(coll, Collections.current.Value, cc, context)
            | None -> false

    let reorder_up (context: LibraryContext) =
        if
            match context with
            | LibraryContext.Playlist (index, id, _) ->
                collections.GetPlaylist(id).Value.MoveChartUp index
            | _ -> false
        then LevelSelect.refresh_all()

    let reorder_down (context: LibraryContext) =
        if
            match context with
            | LibraryContext.Playlist (index, id, _) ->
                collections.GetPlaylist(id).Value.MoveChartDown index
            | _ -> false
        then LevelSelect.refresh_all()

type private CreateFolderPage() as this =
    inherit Page()

    let new_name = Setting.simple "Folder" |> Setting.alphaNum
    let icon = Setting.simple Icons.heart

    do
        this.Content(
            column()
            |+ PageSetting("collections.edit.folder_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PageSetting("collections.edit.icon",
                Selector( CreateFolderPage.Icons,
                icon)).Pos(300.0f)
            |+ PageButton("confirm.yes", 
                (fun () -> if collections.CreateFolder(new_name.Value, icon.Value).IsSome then Collections.select new_name.Value; Menu.Back() )).Pos(400.0f)
        )

    override this.Title = L"collections.create_folder.name"
    override this.OnClose() = ()

    static member Icons = 
        [|
            Icons.heart, Icons.heart
            Icons.star, Icons.star
            Icons.bookmark, Icons.bookmark
            Icons.folder, Icons.folder
        |]

type private CreatePlaylistPage() as this =
    inherit Page()

    let new_name = Setting.simple "Playlist" |> Setting.alphaNum
    let icon = Setting.simple Icons.heart

    do
        this.Content(
            column()
            |+ PageSetting("collections.edit.playlist_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PageSetting("collections.edit.icon",
                Selector( CreatePlaylistPage.Icons,
                icon)).Pos(300.0f)
            |+ PageButton("confirm.yes", 
                (fun () -> if collections.CreatePlaylist(new_name.Value, icon.Value).IsSome then Collections.select new_name.Value; Menu.Back() )).Pos(400.0f)
        )

    override this.Title = L"collections.create_playlist.name"
    override this.OnClose() = ()

    static member Icons =
        [|
            Icons.star, Icons.star
            Icons.goal, Icons.goal
            Icons.play, Icons.play
            Icons.playlist, Icons.playlist
        |]

type private EditFolderPage(name: string, folder: Folder) as this =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphaNum

    do
        let content = 
            column()
            |+ PageSetting("collections.edit.folder_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PageSetting("collections.edit.icon",
                Selector( CreateFolderPage.Icons,
                folder.Icon)).Pos(270.0f)
            |+ PageButton("collections.edit.delete", 
                (fun () -> 
                    ConfirmPage(Localisation.localiseWith [name] "misc.confirmdelete", 
                        fun () ->
                            if collections.Delete name then 
                                if options.LibraryMode.Value = LibraryMode.Collections then LevelSelect.refresh_all()
                                if options.SelectedCollection.Value = Some name then Collections.unselect()
                                Menu.Back()
                    ).Show()
                ),
                Icon = Icons.delete).Pos(370.0f)
            |+ PageButton("collections.edit.select", 
                (fun () -> Collections.select name; Menu.Back()) )
                .Pos(470.0f)
                .Tooltip(Tooltip.Info("collections.edit.select"))

            |+ if options.SelectedCollection.Value = Some name then
                Text(L"collections.selected.this",
                Position = Position.SliceBottom(260.0f).SliceTop(70.0f))
               else
                Text(Localisation.localiseWith [match options.SelectedCollection.Value with Some s -> s | None -> "--"] "collections.selected.other",
                Position = Position.SliceBottom(260.0f).SliceTop(70.0f))
            |+ Text(Localisation.localiseWith [(!|"add_to_collection").ToString()] "collections.addhint",
                Position = Position.SliceBottom(190.0f).SliceTop(70.0f))
            |+ Text(Localisation.localiseWith [(!|"remove_from_collection").ToString()] "collections.removehint",
                Position = Position.SliceBottom(120.0f).SliceTop(70.0f))

        this.Content content

    override this.Title = name
    override this.OnClose() =
        if new_name.Value <> name then
            if collections.RenameCollection(name, new_name.Value) then
                if options.SelectedCollection.Value = Some name then Collections.select new_name.Value
                Logging.Debug (sprintf "Renamed collection '%s' to '%s'" name new_name.Value)
            else Logging.Debug "Rename failed, maybe that name already exists?"

type private EditPlaylistPage(name: string, playlist: Playlist) as this =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphaNum

    do
        let content =
            column()
            |+ PageSetting("collections.edit.playlist_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PageSetting("collections.edit.icon",
                Selector( CreatePlaylistPage.Icons,
                playlist.Icon)).Pos(270.0f)
            |+ PageButton("collections.edit.delete", 
                (fun () -> 
                    ConfirmPage(Localisation.localiseWith [name] "misc.confirmdelete", 
                        fun () -> 
                            if collections.Delete name then 
                                if options.LibraryMode.Value = LibraryMode.Collections then LevelSelect.refresh_all()
                                if options.SelectedCollection.Value = Some name then Collections.unselect()
                                Menu.Back()
                    ).Show()
                ),
                Icon = Icons.delete)
                .Pos(370.0f)
            |+ PageButton("collections.edit.select", 
                (fun () -> Collections.select name; Menu.Back()) )
                .Pos(470.0f)
                .Tooltip(Tooltip.Info("collections.edit.select"))
            
            |+ if options.SelectedCollection.Value = Some name then
                Text(L"collections.selected.this",
                Position = Position.SliceBottom(260.0f).SliceTop(70.0f))
               else
                Text(Localisation.localiseWith [match options.SelectedCollection.Value with Some s -> s | None -> "[None]"] "collections.selected.other",
                Position = Position.SliceBottom(260.0f).SliceTop(70.0f))
            |+ Text(Localisation.localiseWith [(!|"add_to_collection").ToString()] "collections.addhint",
                Position = Position.SliceBottom(190.0f).SliceTop(70.0f))
            |+ Text(Localisation.localiseWith [(!|"remove_from_collection").ToString()] "collections.removehint",
                Position = Position.SliceBottom(120.0f).SliceTop(70.0f))

        this.Content content

    override this.Title = name
    override this.OnClose() =
        if new_name.Value <> name then
            if collections.RenamePlaylist(name, new_name.Value) then
                if options.SelectedCollection.Value = Some name then Collections.select new_name.Value
                Logging.Debug (sprintf "Renamed playlist '%s' to '%s'" name new_name.Value)
            else Logging.Debug "Rename failed, maybe that name already exists?"

type private CollectionButton(icon, name, action) =
    inherit StaticContainer(NodeType.Button (fun () -> Style.click.Play(); action()))
    
    override this.Init(parent: Widget) =
        this
        |+ Text(
            K (sprintf "%s %s  >" icon name),
            Color = ( 
                fun () -> ( 
                    (if this.Focused then Colors.yellow_accent else Colors.white),
                    (if options.SelectedCollection.Value = Some name then Colors.blue_shadow else Colors.shadow_2)
                )
            ),
            Align = Alignment.LEFT,
            Position = Position.Margin Style.PADDING)
        |* Clickable.Focus this
        base.Init parent

    override this.OnFocus() = Style.hover.Play(); base.OnFocus()
    
    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds Colors.yellow_accent.O1
        base.Draw()

type SelectCollectionPage(on_select: (string * Collection) -> unit) as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
    let refresh() =
        container.Clear()
        container
        |+ PageButton("collections.create_folder", (fun () -> Menu.ShowPage CreateFolderPage))
            .Tooltip(Tooltip.Info("collections.create_folder"))
        |+ PageButton("collections.create_playlist", (fun () -> Menu.ShowPage CreatePlaylistPage))
            .Tooltip(Tooltip.Info("collections.create_playlist"))
        |* Dummy()
        for name, collection in collections.List do
            match collection with
            | Folder f -> 
                container.Add( CollectionButton(
                    f.Icon.Value,
                    name,
                    fun () -> on_select(name, collection) )
                )
            | Playlist p ->
                container.Add( CollectionButton(
                    p.Icon.Value,
                    name,
                    fun () -> on_select(name, collection) )
                )
        if container.Focused then container.Focus()

    do
        refresh()

        this.Content( ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f)) )

    override this.Title = L"collections.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh()

    static member Editor() = 
        SelectCollectionPage(
            fun (name, collection) ->
                match collection with
                | Folder f -> EditFolderPage(name, f).Show()
                | Playlist p -> EditPlaylistPage(name, p).Show()
        )