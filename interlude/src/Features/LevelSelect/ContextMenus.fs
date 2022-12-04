namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Data.Charts
open Prelude.Data.Charts.Tables
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Collections
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Options

type ChartContextMenu(cc: CachedChart, context: LibraryContext) as this =
    inherit Page()

    do
        let content = 
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))
            |+ PrettyButton("chart.delete", (fun () -> ChartContextMenu.ConfirmDelete(cc, true)), Icon = Icons.delete)

            |+ PrettyButton(
                "chart.add_to_collection",
                (fun () -> Menu.ShowPage (SelectCollectionPage(
                    fun (name, collection) ->
                        if CollectionManager.add_to(name, collection, cc) then Menu.Back()
                    ))),
                Icon = Icons.add_to_collection
            )

        match context with
        | LibraryContext.None -> ()
        | LibraryContext.Folder name
        | LibraryContext.Playlist (_, name, _) ->
            content
            |* PrettyButton(
                "chart.remove_from_collection",
                (fun () -> 
                    if CollectionManager.remove_from(name, Library.collections.Get(name).Value, cc, context) then Menu.Back()
                ),
                Icon = Icons.remove_from_collection,
                Text = Localisation.localiseWith [name] "options.chart.remove_from_collection.name"
            )
        | LibraryContext.Table lvl ->
            content
            |* PrettyButton(
                "chart.remove_from_collection",
                (fun () -> 
                    if CollectionManager.remove_from(lvl, Level (Table.current().Value.TryLevel(lvl).Value), cc, context) then Menu.Back()
                ),
                Icon = Icons.remove_from_collection,
                Text = Localisation.localiseWith [options.Table.Value.Value] "options.chart.remove_from_collection.name"
            )

        if options.EnableTableEdit.Value then
            
            match Table.current() with
            | Some table -> 
                if not (table.Contains(cc: CachedChart)) then
                    content
                    |* PrettyButton(
                        "chart.add_to_table",
                        (fun () -> SelectTableLevelPage(fun level -> if CollectionManager.add_to(level.Name, Level level, cc) then Menu.Back()) |> Menu.ShowPage),
                        Icon = Icons.add_to_collection,
                        Text = Localisation.localiseWith [table.Name] "options.chart.add_to_table.name"
                    )
            | None -> ()

        this.Content content

    override this.Title = cc.Title
    override this.OnClose() = ()
    
    static member ConfirmDelete(cc, is_submenu) =
        let chartName = sprintf "%s [%s]" cc.Title cc.DiffName
        ConfirmPage(
            Localisation.localiseWith [chartName] "misc.confirmdelete",
            fun () ->
                Library.delete cc
                LevelSelect.refresh <- true
                if is_submenu then Menu.Back()
            ) |> Menu.ShowPage

type GroupContextMenu(name: string, charts: CachedChart seq, context: LibraryGroupContext) as this =
    inherit Page()

    do
        let content = 
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))
            |+ PrettyButton("group.delete", (fun () -> GroupContextMenu.ConfirmDelete(name, charts, true)), Icon = Icons.delete)
        this.Content content

    override this.Title = name
    override this.OnClose() = ()
    
    static member ConfirmDelete(name, charts, is_submenu) =
        let groupName = sprintf "%s (%i charts)" name (Seq.length charts)
        ConfirmPage(
            Localisation.localiseWith [groupName] "misc.confirmdelete",
            fun () ->
                Library.deleteMany charts
                LevelSelect.refresh <- true
                if is_submenu then Menu.Back()
            ) |> Menu.ShowPage

    static member Show(name, charts, context) =
        match context with
        | LibraryGroupContext.None -> GroupContextMenu(name, charts, context) |> Menu.ShowPage
        | LibraryGroupContext.Folder id -> EditFolderPage(id, Library.collections.GetFolder(id).Value) |> Menu.ShowPage
        | LibraryGroupContext.Playlist id -> EditPlaylistPage(id, Library.collections.GetPlaylist(id).Value) |> Menu.ShowPage
        | LibraryGroupContext.Table lvl -> (EditLevelPage (Table.current().Value.TryLevel(lvl).Value)) |> Menu.ShowPage