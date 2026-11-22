namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.UserDatabase
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Collections

type BatchLikesContextMenu(charts: (ChartMeta * LibraryContext) seq) =
    inherit Page()

    let confirm_bulk_delete () =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_delete",
            fun () ->
                Content.Charts.Delete(charts |> Seq.map fst)
                LevelSelect.refresh_all()

                Menu.Back()
        )
            .Show()

    let confirm_bulk_unlike () =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_unlike",
            fun () ->
                charts |> Seq.map(fst >> _.Hash) |> Seq.iter Content.Library.Collections.Unlike
                LevelSelect.refresh_all()

                Menu.Back()
        )
            .Show()

    let add_to_collection () =
        SelectCollectionsPage(
            (fun (_, collection) ->
                match collection with
                | Playlist p ->
                    for chart_meta, ctx in charts do
                        match ctx with
                        | LibraryContext.Playlist(_, _, data) -> p.Add(chart_meta, data.Rate.Value, data.Mods.Value)
                        | _ -> p.Add(chart_meta, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
                        |> ignore
                | Folder f ->
                    for chart_meta, _ in charts do
                        f.Add(chart_meta) |> ignore

                LevelSelect.refresh_all()
                Menu.Exit()
            ),
            K false,
            true
        )
            .Show()

    override this.Content() : Widget =
        page_container()
            .With(
                PageButton(%"bulk_actions.unlike", confirm_bulk_unlike).Icon(Icons.FOLDER_MINUS).Hotkey("like").Pos(0),
                PageButton(%"bulk_actions.add_to_collection", add_to_collection).Icon(Icons.FOLDER_PLUS).Pos(2),
                PageButton(%"bulk_actions.export_oszs", fun () -> BulkActions.confirm_bulk_export charts)
                    .Icon(Icons.UPLOAD)
                    .Pos(4),
                PageButton(%"bulk_actions.delete", confirm_bulk_delete)
                    .TextColor(Colors.red_accent)
                    .Icon(Icons.TRASH)
                    .Hotkey("delete")
                    .Pos(6)
            )

    override this.Title = %"bulk_actions"
