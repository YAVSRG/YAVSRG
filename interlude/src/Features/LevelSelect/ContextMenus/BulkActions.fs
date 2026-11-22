namespace Interlude.Features.LevelSelect

open Prelude
open Prelude.Data.Library
open Interlude.Content
open Interlude.UI
open Interlude.Features.Export

module BulkActions =

    let confirm_bulk_delete (charts: (ChartMeta * LibraryContext) seq) : unit =
        ConfirmPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_delete",
            fun () ->
                // delete charts selected from packs, just from their packs
                charts
                |> Seq.choose(
                    function
                    | chart_meta, LibraryContext.Pack pack_name -> Some(chart_meta, pack_name)
                    | _ -> None
                )
                |> Seq.groupBy snd
                |> Seq.iter(fun (pack_name, charts_in_pack) ->
                    Content.Charts.RemoveFromPack(charts_in_pack |> Seq.map fst, pack_name)
                )

                // delete charts picked by custom grouping, from all packs
                Content.Charts.Delete(
                    charts
                    |> Seq.filter(
                        function
                        | _, LibraryContext.Pack _ -> false
                        | _ -> true
                    )
                    |> Seq.map fst
                )

                LevelSelect.refresh_all()
                Menu.Back()
        )
            .Show()

    let confirm_bulk_export (charts: (ChartMeta * LibraryContext) seq) : unit =
        OsuExportOptionsPage(
            [ (Seq.length charts).ToString() ] %> "bulk_actions.confirm_bulk_export",
            Map.empty,
            fun _ options ->
                let loaded_charts =
                    seq {
                        for chart_meta, _ in charts do
                            yield Content.Library.Charts.GetChart(chart_meta.Hash), chart_meta
                    }

                OsuExport.bulk_export loaded_charts options
        )
            .Show()
