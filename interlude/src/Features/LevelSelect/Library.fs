namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Common
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Collections
open Prelude.Data.Charts.Sorting
open Interlude.Options
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Gameplay

module CollectionManager =

    let add_to (name: string, collection: Collection, cc: CachedChart) =
        if
            match collection with
            | Folder c -> c.Add cc
            | Playlist p -> p.Add (cc, rate.Value, selectedMods.Value)
        then
            if options.LibraryMode.Value = LibraryMode.Collections then LevelSelect.refresh <- true else LevelSelect.minorRefresh <- true
            Notifications.add (Localisation.localiseWith [cc.Title; name] "collections.added", NotificationType.Info)
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
            if options.LibraryMode.Value = LibraryMode.Collections then LevelSelect.refresh <- true else LevelSelect.minorRefresh <- true
            Notifications.add (Localisation.localiseWith [cc.Title; name] "collections.removed", NotificationType.Info)
            if Some cc = Chart.cacheInfo then Chart.context <- LibraryContext.None
            true
        else false

    module Current =

        let quick_add(cc: CachedChart) =
            add_to (options.SelectedCollection.Value, Collections.current, cc)

        let quick_remove(cc: CachedChart, context: LibraryContext) =
            remove_from(options.SelectedCollection.Value, Collections.current, cc, context)


    let reorder_up (context: LibraryContext) =
        if
            match context with
            | LibraryContext.Playlist (index, id, _) ->
                collections.GetPlaylist(id).Value.MoveChartUp index
            | _ -> false
        then LevelSelect.refresh <- true

    let reorder_down (context: LibraryContext) =
        if
            match context with
            | LibraryContext.Playlist (index, id, _) ->
                collections.GetPlaylist(id).Value.MoveChartDown index
            | _ -> false
        then LevelSelect.refresh <- true

type private CreateFolderPage() as this =
    inherit Page()

    let new_name = Setting.simple "Folder" |> Setting.alphaNum
    let icon = Setting.simple Icons.heart

    do
        this.Content(
            column()
            |+ PrettySetting("collections.edit.folder_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettySetting("collections.edit.icon",
                Selector( CreateFolderPage.Icons,
                icon)).Pos(300.0f)
            |+ PrettyButton("confirm.yes", 
                (fun () -> if collections.CreateFolder(new_name.Value, icon.Value).IsSome then Menu.Back() )).Pos(400.0f)
        )

    override this.Title = N"collections.create_folder"
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
            |+ PrettySetting("collections.edit.playlist_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettySetting("collections.edit.icon",
                Selector( CreatePlaylistPage.Icons,
                icon)).Pos(300.0f)
            |+ PrettyButton("confirm.yes", 
                (fun () -> if collections.CreatePlaylist(new_name.Value, icon.Value).IsSome then Menu.Back() )).Pos(400.0f)
        )

    override this.Title = N"collections.create_playlist"
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
        this.Content(
            column()
            |+ PrettySetting("collections.edit.folder_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettySetting("collections.edit.icon",
                Selector( CreateFolderPage.Icons,
                folder.Icon)).Pos(300.0f)
            |+ PrettyButton("collections.edit.delete", 
                (fun () -> 
                    Menu.ShowPage (ConfirmPage(Localisation.localiseWith [name] "misc.confirmdelete", fun () -> collections.Delete name |> ignore; Menu.Back() ))),
                Icon = Icons.delete).Pos(400.0f)
        )

    override this.Title = name
    override this.OnClose() =
        if new_name.Value <> name then
            if collections.RenameCollection(name, new_name.Value) then
                Logging.Debug (sprintf "Renamed collection '%s' to '%s'" name new_name.Value)
            else Logging.Debug "Rename failed, maybe that name already exists?"

type private EditPlaylistPage(name: string, playlist: Playlist) as this =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphaNum

    do
        this.Content(
            column()
            |+ PrettySetting("collections.edit.playlist_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettySetting("collections.edit.icon",
                Selector( CreatePlaylistPage.Icons,
                playlist.Icon)).Pos(300.0f)
            |+ PrettyButton("collections.edit.delete", 
                (fun () -> 
                    Menu.ShowPage (ConfirmPage(Localisation.localiseWith [name] "misc.confirmdelete", fun () -> collections.Delete name |> ignore; Menu.Back() ))),
                Icon = Icons.delete).Pos(400.0f)
        )

    override this.Title = name
    override this.OnClose() =
        if new_name.Value <> name then
            if collections.RenamePlaylist(name, new_name.Value) then
                Logging.Debug (sprintf "Renamed playlist '%s' to '%s'" name new_name.Value)
            else Logging.Debug "Rename failed, maybe that name already exists?"

type private CollectionButton(icon, name, action) =
    inherit StaticContainer(NodeType.Button (fun _ -> action()))
    
    override this.Init(parent: Widget) =
        this
        |+ Text(
            K (sprintf "%s %s  >" icon name),
            Color = ( 
                fun () -> ( (if this.Focused then Style.color(255, 1.0f, 0.5f) else Color.White), Color.Black )
            ),
            Align = Alignment.LEFT,
            Position = Position.Margin(Style.padding))
        |* Clickable(this.Select, OnHover = fun b -> if b then this.Focus())
        base.Init parent
    
    override this.Draw() =
        if this.Focused then Draw.rect this.Bounds (!*Palette.HOVER)
        base.Draw()

type SelectCollectionPage(on_select: (string * Collection) -> unit) as this =
    inherit Page()

    let container = FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
    let refresh() =
        container.Clear()
        container
        |+ PrettyButton("collections.create_folder", (fun () -> Menu.ShowPage CreateFolderPage))
        |+ PrettyButton("collections.create_playlist", (fun () -> Menu.ShowPage CreatePlaylistPage))
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

        this.Content(
            SwitchContainer.Column<Widget>()
            |+ ScrollContainer.Flow(container, Position = Position.Margin(100.0f, 200.0f))
            |+ Text(Localisation.localiseWith [(!|"add_to_collection").ToString()] "collections.addhint",
                Position = Position.SliceBottom(190.0f).SliceTop(70.0f))
            |+ Text(Localisation.localiseWith [(!|"remove_from_collection").ToString()] "collections.removehint",
                Position = Position.SliceBottom(120.0f).SliceTop(70.0f))
        )

    override this.Title = N"collections"
    override this.OnClose() = ()
    override this.OnReturnTo() = refresh()

    static member Editor() = 
        SelectCollectionPage(
            fun (name, collection) ->
                match collection with
                | Folder f -> Menu.ShowPage(EditFolderPage(name, f))
                | Playlist p -> Menu.ShowPage(EditPlaylistPage(name, p))
        )

type private ModeDropdown(options: string seq, label: string, setting: Setting<string>, reverse: Setting<bool>, bind: Hotkey) =
    inherit StaticContainer(NodeType.None)

    override this.Init(parent: Widget) =
        this 
        |+ StylishButton(
            ( fun () -> this.ToggleDropdown() ),
            K (label + ":"),
            Style.highlight 100,
            Hotkey = bind,
            Position = Position.SliceLeft 120.0f)
        |* StylishButton(
            ( fun () -> reverse.Value <- not reverse.Value ),
            ( fun () -> sprintf "%s %s" setting.Value (if reverse.Value then Icons.order_descending else Icons.order_ascending) ),
            Style.dark 100,
            // todo: hotkey for direction reversal
            Position = Position.TrimLeft 145.0f )
        base.Init parent

    member this.ToggleDropdown() =
        match this.Dropdown with
        | Some _ -> this.Dropdown <- None
        | _ ->
            let d = Dropdown.Selector options id (fun g -> setting.Set g) (fun () -> this.Dropdown <- None)
            d.Position <- Position.SliceTop(d.Height + 60.0f).TrimTop(60.0f).Margin(Style.padding, 0.0f)
            d.Init this
            this.Dropdown <- Some d

    member val Dropdown : Dropdown option = None with get, set

    override this.Draw() =
        base.Draw()
        match this.Dropdown with
        | Some d -> d.Draw()
        | None -> ()

    override this.Update(elapsedTime, moved) =
        base.Update(elapsedTime, moved)
        match this.Dropdown with
        | Some d -> d.Update(elapsedTime, moved)
        | None -> ()
    
type LibraryModeSettings() =
    inherit StaticContainer(NodeType.None)

    let group_selector = 
        ModeDropdown(
            groupBy.Keys,
            "Group",
            options.ChartGroupMode |> Setting.trigger (fun _ -> LevelSelect.refresh <- true),
            options.ChartGroupReverse |> Setting.trigger (fun _ -> LevelSelect.refresh <- true),
            "group_mode"
        ).Tooltip(L"levelselect.groupby.tooltip")

    let manage_collections =
        StylishButton(
            (fun () -> Menu.ShowPage SelectCollectionPage.Editor),
            K (sprintf "%s %s" Icons.collections (L"levelselect.collections.name")),
            Style.main 100,
            Hotkey = "group_mode"
        ).Tooltip(L"levelselect.collections.tooltip")
        
    let manage_tables =
        StylishButton(
            ignore,
            K (sprintf "%s %s" Icons.edit (L"levelselect.table.name")),
            Style.main 100,
            Hotkey = "group_mode"
        ).Tooltip(L"levelselect.table.tooltip")

    let swap = SwapContainer(Position = { Left = 0.8f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 1.0f %+ 0.0f; Bottom = 0.0f %+ 170.0f })

    let update_swap() =
        swap.Current <-
        match options.LibraryMode.Value with
        | LibraryMode.All -> group_selector
        | LibraryMode.Collections -> manage_collections
        | LibraryMode.Table -> manage_tables

    override this.Init(parent) =
        this
        |+ StylishButton.Selector(
            sprintf "%s %s:" Icons.collections (L"levelselect.librarymode"),
            [|
                LibraryMode.All, L"levelselect.librarymode.all"
                LibraryMode.Collections, L"levelselect.librarymode.collections"
                LibraryMode.Table, L"levelselect.librarymode.table"
            |],
            options.LibraryMode |> Setting.trigger (fun _ -> LevelSelect.refresh <- true; update_swap()),
            Style.dark 100,
            Hotkey = "collections")
            .Tooltip(L"levelselect.librarymode.tooltip")
            .WithPosition { Left = 0.4f %+ 25.0f; Top = 0.0f %+ 120.0f; Right = 0.6f %- 25.0f; Bottom = 0.0f %+ 170.0f }
        
        |+ ModeDropdown(sortBy.Keys, "Sort",
            options.ChartSortMode |> Setting.trigger (fun _ -> LevelSelect.refresh <- true),
            options.ChartSortReverse |> Setting.map not not |> Setting.trigger (fun _ -> LevelSelect.refresh <- true),
            "sort_mode")
            .Tooltip(L"levelselect.sortby.tooltip")
            .WithPosition { Left = 0.6f %+ 0.0f; Top = 0.0f %+ 120.0f; Right = 0.8f %- 25.0f; Bottom = 0.0f %+ 170.0f }
        
        |* swap

        update_swap()

        base.Init(parent)
        
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

        if Chart.cacheInfo.IsSome then

            if (!|"add_to_collection").Tapped() then CollectionManager.Current.quick_add(Chart.cacheInfo.Value) |> ignore
            elif (!|"remove_from_collection").Tapped() then CollectionManager.Current.quick_remove(Chart.cacheInfo.Value, Chart.context) |> ignore
            elif (!|"move_up_in_collection").Tapped() then CollectionManager.reorder_up(Chart.context)
            elif (!|"move_down_in_collection").Tapped() then CollectionManager.reorder_down(Chart.context)