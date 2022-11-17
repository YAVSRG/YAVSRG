namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Library
open Prelude.Data.Charts.Collections
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Options

module CollectionManager =

    module Current =

        let quick_add(cc: CachedChart, context: LibraryContext) =
            if 
                match Collections.current with
                | Collection c -> c.Add cc
                | Playlist p -> p.Add (cc, rate.Value, selectedMods.Value)
            then
                if options.ChartGroupMode.Value = "Collections" then LevelSelect.refresh <- true else LevelSelect.minorRefresh <- true
                Notifications.add (Localisation.localiseWith [Chart.cacheInfo.Value.Title; options.SelectedCollection.Value] "collections.added", NotificationType.Info)

        let quick_remove(cc: CachedChart, context: LibraryContext) =
            if
                match Collections.current with
                | Collection c -> c.Remove cc
                | Playlist p ->
                    match context with
                    | LibraryContext.Playlist (i, name, _) when name = options.SelectedCollection.Value -> p.RemoveAt i
                    | _ -> p.RemoveSingle cc
            then
                if options.ChartGroupMode.Value = "Collections" then LevelSelect.refresh <- true else LevelSelect.minorRefresh <- true
                Notifications.add (Localisation.localiseWith [Chart.cacheInfo.Value.Title; options.SelectedCollection.Value] "collections.removed", NotificationType.Info)
                if context = Chart.context then Chart.context <- LibraryContext.None

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

type private CreateCollectionPage() as this =
    inherit Page()

    let new_name = Setting.simple "Collection" |> Setting.alphaNum
    let icon = Setting.simple Icons.heart

    do
        this.Content(
            column()
            |+ PrettySetting("collections.edit.collection_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettySetting("collections.edit.icon",
                Selector( [|
                    Icons.heart, Icons.heart
                    Icons.star, Icons.star
                    Icons.folder, Icons.folder
                    |],
                icon)).Pos(300.0f)
            |+ PrettyButton("confirm.yes", 
                (fun () -> if collections.CreateCollection(new_name.Value, icon.Value).IsSome then Menu.Back() )).Pos(400.0f)
        )

    override this.Title = N"collections.create_collection"
    override this.OnClose() = ()

type private CreatePlaylistPage() as this =
    inherit Page()

    let new_name = Setting.simple "Playlist" |> Setting.alphaNum
    let icon = Setting.simple Icons.heart

    do
        this.Content(
            column()
            |+ PrettySetting("collections.edit.playlist_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettySetting("collections.edit.icon",
                Selector( [|
                    Icons.star, Icons.star
                    Icons.goal, Icons.goal
                    Icons.play, Icons.play
                    Icons.folder, Icons.folder
                    |],
                icon)).Pos(300.0f)
            |+ PrettyButton("confirm.yes", 
                (fun () -> if collections.CreatePlaylist(new_name.Value, icon.Value).IsSome then Menu.Back() )).Pos(400.0f)
        )

    override this.Title = N"collections.create_playlist"
    override this.OnClose() = ()

type private EditCollectionPage(name: string, collection: Collection) as this =
    inherit Page()

    let new_name = Setting.simple name |> Setting.alphaNum

    do
        this.Content(
            column()
            |+ PrettySetting("collections.edit.collection_name", TextEntry(new_name, "none")).Pos(200.0f)
            |+ PrettySetting("collections.edit.icon",
                Selector( [|
                    Icons.heart, Icons.heart
                    Icons.star, Icons.star
                    Icons.folder, Icons.folder
                    |],
                collection.Icon)).Pos(300.0f)
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
                Selector( [|
                    Icons.star, Icons.star
                    Icons.goal, Icons.goal
                    Icons.play, Icons.play
                    Icons.folder, Icons.folder
                    |],
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

type private CollectionsPage() as this =
    inherit Page()

    do
        let container =
            FlowContainer.Vertical<Widget>(PRETTYHEIGHT)
            |+ PrettyButton("collections.create_collection", (fun () -> Menu.ShowPage CreateCollectionPage))
            |+ PrettyButton("collections.create_playlist", (fun () -> Menu.ShowPage CreatePlaylistPage))
            |+ Dummy()
        for name, collection in collections.List do
            match collection with
            | Collection c -> 
                container.Add( PrettyButton("collections.collection",
                    (fun () -> Menu.ShowPage(EditCollectionPage(name, c))),
                    Icon = c.Icon.Value, Text = name) )
            | Playlist p ->
                container.Add( PrettyButton("collections.playlist",
                    (fun () -> Menu.ShowPage(EditPlaylistPage(name, p))),
                    Icon = p.Icon.Value, Text = name) )

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
    
type CollectionManager() =
    inherit StylishButton ((fun () -> Menu.ShowPage CollectionsPage), K (sprintf "%s %s" Icons.collections (N"collections")), (fun () -> Style.color(100, 0.5f, 0.0f)), Hotkey = "collections")
    
    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)

        if Chart.cacheInfo.IsSome then

            if (!|"add_to_collection").Tapped() then CollectionManager.Current.quick_add(Chart.cacheInfo.Value, Chart.context)
            elif (!|"remove_from_collection").Tapped() then CollectionManager.Current.quick_remove(Chart.cacheInfo.Value, Chart.context)
            elif (!|"move_up_in_collection").Tapped() then CollectionManager.reorder_up(Chart.context)
            elif (!|"move_down_in_collection").Tapped() then CollectionManager.reorder_down(Chart.context)