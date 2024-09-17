namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library
open Prelude.Data.Library.Collections
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Collections

type BatchLikesContextMenu(charts: (ChartMeta * LibraryContext) seq) =
    inherit Page()

    let confirm_bulk_delete() =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_delete",
            fun () ->
                ChartDatabase.delete_many (charts |> Seq.map fst) Content.Charts
                LevelSelect.refresh_all ()

                Menu.Back()
        )
            .Show()

    let confirm_bulk_unlike() =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_unlike",
            fun () ->
                charts |> Seq.map (fst >> _.Hash) |> Seq.iter Content.Library.Collections.Unlike
                LevelSelect.refresh_all ()

                Menu.Back()
        )
            .Show()

    let add_to_collection() =
        SelectCollectionPage(
            (fun (_, collection) ->
                match collection with
                | Playlist p ->
                    for cc, ctx in charts do
                        match ctx with
                        | LibraryContext.Playlist (_, _, data) ->
                            p.Add(cc, data.Rate.Value, data.Mods.Value)
                        | _ -> p.Add(cc, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
                        |> ignore
                | Folder f ->
                    for cc, _ in charts do f.Add cc |> ignore
                LevelSelect.refresh_all ()
                Menu.Exit()
            ), 
            K false, 
            true
        )
            .Show()

    override this.Content() =
        page_container()
        |+ PageButton(%"bulk_actions.unlike", confirm_bulk_unlike, Icon = Icons.FOLDER_MINUS, Hotkey = %%"unlike").Pos(0)
        |+ PageButton(%"bulk_actions.add_to_collection", add_to_collection, Icon = Icons.FOLDER_PLUS).Pos(2)
        |+ PageButton(%"bulk_actions.delete", confirm_bulk_delete, Icon = Icons.TRASH, Hotkey = %%"delete").Pos(4)
        :> Widget

    override this.Title = %"bulk_actions"
    override this.OnClose() = ()

type BatchFolderContextMenu(folder: string, charts: (ChartMeta * LibraryContext) seq) =
    inherit Page()

    let confirm_bulk_remove() =
        ConfirmPage(
            [ (Seq.length charts).ToString(); folder ] %> "bulk_actions.confirm_bulk_remove",
            fun () ->
                match Content.Library.Collections.GetFolder folder with
                | Some f -> for cc, _ in charts do f.Remove cc |> ignore
                | _ -> ()
                LevelSelect.refresh_all ()
                Menu.Back()
        )
            .Show()

    let confirm_bulk_like() =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_like",
            fun () ->
                charts |> Seq.map (fst >> _.Hash) |> Seq.iter Content.Library.Collections.Like
                LevelSelect.refresh_all ()

                Menu.Back()
        )
            .Show()

    let add_to_collection() =
        SelectCollectionPage(
            (fun (_, collection) ->
                match collection with
                | Playlist p ->
                    for cc, ctx in charts do
                        match ctx with
                        | LibraryContext.Playlist (_, _, data) ->
                            p.Add(cc, data.Rate.Value, data.Mods.Value)
                        | _ -> p.Add(cc, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
                        |> ignore
                | Folder f ->
                    for cc, _ in charts do f.Add cc |> ignore
                LevelSelect.refresh_all ()
                Menu.Exit()
            ), 
            (function (f, Folder _) when f = folder -> false | _ -> true), 
            true
        )
            .Show()

    override this.Content() =
        page_container()
        |+ PageButton(%"bulk_actions.like", confirm_bulk_like, Icon = Icons.HEART, Hotkey = %%"like").Pos(0)
        |+ PageButton(%"bulk_actions.add_to_collection", add_to_collection, Icon = Icons.FOLDER_PLUS).Pos(2)
        |+ PageButton(%"bulk_actions.remove_from_folder", confirm_bulk_remove, Icon = Icons.FOLDER_MINUS).Pos(4)
        :> Widget

    override this.Title = %"bulk_actions"
    override this.OnClose() = ()

type BatchPlaylistContextMenu(playlist: string, charts: (ChartMeta * LibraryContext) seq) =
    inherit Page()

    let confirm_bulk_remove() =
        ConfirmPage(
            [ (Seq.length charts).ToString(); playlist ] %> "bulk_actions.confirm_bulk_remove",
            fun () ->
                match Content.Library.Collections.GetPlaylist playlist with
                | Some p -> 
                    charts 
                    |> Seq.map (function _, LibraryContext.Playlist(index, _, _) -> index | _ -> -1)
                    |> Seq.sortDescending
                    |> Seq.iter (p.RemoveAt >> ignore)

                | _ -> ()
                LevelSelect.refresh_all ()

                Menu.Back()
        )
            .Show()

    let confirm_bulk_like() =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_like",
            fun () ->
                charts |> Seq.map (fst >> _.Hash) |> Seq.iter Content.Library.Collections.Like
                LevelSelect.refresh_all ()

                Menu.Back()
        )
            .Show()

    let add_to_collection() =
        SelectCollectionPage(
            (fun (_, collection) ->
                match collection with
                | Playlist p ->
                    for cc, ctx in charts do
                        match ctx with
                        | LibraryContext.Playlist (_, _, data) ->
                            p.Add(cc, data.Rate.Value, data.Mods.Value)
                        | _ -> p.Add(cc, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
                        |> ignore
                | Folder f ->
                    for cc, _ in charts do f.Add cc |> ignore
                LevelSelect.refresh_all ()
                Menu.Exit()
            ), 
            (function (f, Folder _) when f = playlist -> false | _ -> true), 
            true
        )
            .Show()

    override this.Content() =
        page_container()
        |+ PageButton(%"bulk_actions.like", confirm_bulk_like, Icon = Icons.HEART, Hotkey = %%"like").Pos(0)
        |+ PageButton(%"bulk_actions.add_to_collection", add_to_collection, Icon = Icons.FOLDER_PLUS).Pos(2)
        |+ PageButton(%"bulk_actions.remove_from_folder", confirm_bulk_remove, Icon = Icons.FOLDER_MINUS).Pos(4)
        :> Widget

    override this.Title = %"bulk_actions"
    override this.OnClose() = ()

type BatchContextMenu(charts: (ChartMeta * LibraryContext) seq) =
    inherit Page()

    let confirm_bulk_delete() =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_delete",
            fun () ->
                // delete charts selected from packs, just from their packs
                charts
                |> Seq.choose (function cc, LibraryContext.Pack p -> Some (cc, p) | _ -> None)
                |> Seq.groupBy snd
                |> Seq.iter (fun (pack, charts_in_pack) -> ChartDatabase.delete_many_from_pack (charts_in_pack |> Seq.map fst) pack Content.Charts)

                // delete charts picked by custom grouping, from all packs
                ChartDatabase.delete_many (charts |> Seq.filter (function cc, LibraryContext.Pack _ -> false | _ -> true ) |> Seq.map fst) Content.Charts

                LevelSelect.refresh_all ()
                Menu.Back()
        )
            .Show()

    let confirm_bulk_like() =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_like",
            fun () ->
                charts |> Seq.map (fst >> _.Hash) |> Seq.iter Content.Library.Collections.Like
                LevelSelect.refresh_all ()

                Menu.Back()
        )
            .Show()

    let add_to_collection() =
        SelectCollectionPage(
            (fun (_, collection) ->
                match collection with
                | Playlist p ->
                    for cc, ctx in charts do
                        match ctx with
                        | LibraryContext.Playlist (_, _, data) ->
                            p.Add(cc, data.Rate.Value, data.Mods.Value)
                        | _ -> p.Add(cc, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
                        |> ignore
                | Folder f ->
                    for cc, _ in charts do f.Add cc |> ignore
                LevelSelect.refresh_all ()
                Menu.Exit()
            ), 
            K false, 
            true
        )
            .Show()

    override this.Content() =
        page_container()
        |+ PageButton(%"bulk_actions.like", confirm_bulk_like, Icon = Icons.HEART, Hotkey = %%"like").Pos(0)
        |+ PageButton(%"bulk_actions.add_to_collection", add_to_collection, Icon = Icons.FOLDER_PLUS).Pos(2)
        |+ PageButton(%"bulk_actions.delete", confirm_bulk_delete, Icon = Icons.TRASH, Hotkey = %%"delete").Pos(4)
        :> Widget

    override this.Title = %"bulk_actions"
    override this.OnClose() = ()