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
            |+ PageButton("chart.delete", (fun () -> ChartContextMenu.ConfirmDelete(cc, true)), Icon = Icons.delete)

            |+ PageButton(
                "chart.add_to_collection",
                (fun () -> 
                    SelectCollectionPage(
                        fun (name, collection) ->
                            if CollectionManager.add_to(name, collection, cc) then Menu.Back()
                    ).Show()
                ),
                Icon = Icons.add_to_collection
            )

        match context with
        | LibraryContext.None
        | LibraryContext.Table _ -> ()
        | LibraryContext.Folder name
        | LibraryContext.Playlist (_, name, _) ->
            content
            |* PageButton(
                "chart.remove_from_collection",
                (fun () -> 
                    if CollectionManager.remove_from(name, Library.collections.Get(name).Value, cc, context) then Menu.Back()
                ),
                Icon = Icons.remove_from_collection,
                Text = Localisation.localiseWith [name] "chart.remove_from_collection.name"
            )

        this.Content content

    override this.Title = cc.Title
    override this.OnClose() = ()
    
    static member ConfirmDelete(cc, is_submenu) =
        let chartName = sprintf "%s [%s]" cc.Title cc.DiffName
        ConfirmPage(
            Localisation.localiseWith [chartName] "misc.confirmdelete",
            fun () ->
                Library.delete cc
                LevelSelect.refresh_all()
                if is_submenu then Menu.Back()
        ).Show()

type GroupContextMenu(name: string, charts: CachedChart seq, context: LibraryGroupContext) as this =
    inherit Page()

    do
        let content = 
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))
            |+ PageButton("group.delete", (fun () -> GroupContextMenu.ConfirmDelete(name, charts, true)), Icon = Icons.delete)
                .Tooltip(Tooltip.Info("group.delete"))
        this.Content content

    override this.Title = name
    override this.OnClose() = ()
    
    static member ConfirmDelete(name, charts, is_submenu) =
        let groupName = sprintf "%s (%i charts)" name (Seq.length charts)
        ConfirmPage(
            Localisation.localiseWith [groupName] "misc.confirmdelete",
            fun () ->
                Library.deleteMany charts
                LevelSelect.refresh_all()
                if is_submenu then Menu.Back()
        ).Show()

    static member Show(name, charts, context) =
        match context with
        | LibraryGroupContext.None -> GroupContextMenu(name, charts, context).Show()
        | LibraryGroupContext.Folder id -> EditFolderPage(id, Library.collections.GetFolder(id).Value).Show()
        | LibraryGroupContext.Playlist id -> EditPlaylistPage(id, Library.collections.GetPlaylist(id).Value).Show()
        | LibraryGroupContext.Table lvl -> EditLevelPage(Table.current().Value.TryLevel(lvl).Value).Show()