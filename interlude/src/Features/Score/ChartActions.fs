namespace Interlude.Features.Score

open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Charts.Processing
open Interlude.Content
open Interlude.UI
open Interlude.Features.Collections
open Interlude.Features.Export

#nowarn "40"

type ScoreChartContextMenu(score_info: ScoreInfo) =
    inherit Page()

    let rec like_button =
        PageButton(
            %"chart.add_to_likes",
            (fun () -> CollectionActions.like_chart score_info.ChartMeta; like_button_swap.Current <- unlike_button),
            Icon = Icons.HEART,
            Hotkey = %%"like"
        )
    and unlike_button =
        PageButton(
            %"chart.remove_from_likes",
            (fun () -> CollectionActions.unlike_chart score_info.ChartMeta; like_button_swap.Current <- like_button),
            Icon = Icons.FOLDER_MINUS,
            Hotkey = %%"unlike"
        )
    and like_button_swap : SwapContainer = SwapContainer(if CollectionActions.is_liked score_info.ChartMeta then unlike_button else like_button)

    override this.Content() =
        let content =
            FlowContainer.Vertical<Widget>(PRETTYHEIGHT, Position = Position.Shrink(PRETTY_MARGIN_X, PRETTY_MARGIN_Y).SliceL(PRETTYWIDTH))
            |+ like_button_swap
            |+ PageButton(
                %"chart.add_to_collection",
                (fun () -> AddToCollectionPage(score_info.ChartMeta).Show()),
                Icon = Icons.FOLDER_PLUS
            )
            |+ PageButton(
                %"chart.delete",
                fun () ->
                    let chart_name = sprintf "%s [%s]" score_info.ChartMeta.Title score_info.ChartMeta.DifficultyName
                    ConfirmPage(
                        [ chart_name ] %> "misc.confirmdelete",
                        fun () -> ChartDatabase.delete score_info.ChartMeta Content.Charts
                    )
                        .Show()
                , Icon = Icons.TRASH, Hotkey = %%"delete"
            )
            |+ PageButton.Once(
                %"chart.export_osz",
                (fun () ->
                    OsuExportOptionsPage(
                        %"chart.export_osz",
                        score_info.WithMods.ModsApplied,
                        function
                        | true -> OsuExport.export_chart_with_mods score_info.WithMods score_info.ChartMeta
                        | false -> OsuExport.export_chart_without_mods score_info.Chart score_info.ChartMeta
                    )
                        .Show()
                ),
                Icon = Icons.UPLOAD
            )

        content

    override this.Title = score_info.ChartMeta.Title
    override this.OnClose() = ()