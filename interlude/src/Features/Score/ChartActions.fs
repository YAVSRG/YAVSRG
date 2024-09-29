namespace Interlude.Features.Score

open Percyqaz.Common
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User
open Prelude.Data.Library
open Prelude.Charts.Processing
open Prelude.Gameplay.Mods
open Interlude.Content
open Interlude.UI
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Online

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

    let export_osz() =
        match Exports.create_osz score_info.Chart score_info.ChartMeta (get_game_folder "Exports") with
        | Ok _ ->
            open_directory (get_game_folder "Exports")
            Notifications.action_feedback(Icons.CHECK, %"notification.song_exported.title", "")
        | Error err ->
            Notifications.error(%"notification.song_export_failed.title", %"notification.song_export_failed.body")
            Logging.Error(sprintf "Error exporting '%s' as osz" score_info.ChartMeta.Title, err)

    let export_osz_with_mods() =
        let mod_string =
            score_info.WithMods.ModsApplied
            |> Mods.in_priority_order
            |> Seq.map (fun (id, _, state) -> Mods.name id (Some state))
            |> String.concat ", "
        let chart_with_mods : Charts.Chart = 
            { Keys = score_info.WithMods.Keys; Notes = score_info.WithMods.Notes; SV = score_info.WithMods.SV; BPM = score_info.WithMods.BPM }
        let meta_with_mods =
            { score_info.ChartMeta with DifficultyName = score_info.ChartMeta.DifficultyName.Trim() + sprintf " (+%s)" mod_string }
        match Exports.create_osz chart_with_mods meta_with_mods (get_game_folder "Exports") with
        | Ok _ ->
            open_directory (get_game_folder "Exports")
            Notifications.action_feedback(Icons.CHECK, %"notification.song_exported.title", "")
        | Error err ->
            Notifications.error(%"notification.song_export_failed.title", %"notification.song_export_failed.body")
            Logging.Error(sprintf "Error exporting '%s' as osz" score_info.ChartMeta.Title, err)

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
                export_osz,
                Icon = Icons.UPLOAD
            )
            |+ PageButton.Once(
                %"chart.export_osz_mods",
                export_osz_with_mods,
                Icon = Icons.UPLOAD
            )
                .Conditional(fun () -> not score_info.WithMods.ModsApplied.IsEmpty)

        match Content.Table with
        | Some table ->
            if Network.status = Network.Status.LoggedIn && score_info.ChartMeta.Keys = table.Info.Keymode then
                content
                |* PageButton(
                    %"chart.suggest_for_table",
                    (fun () -> SuggestChartPage(table, score_info.ChartMeta.Hash).Show()),
                    Icon = Icons.SIDEBAR
                )
        | _ -> ()

        content

    override this.Title = score_info.ChartMeta.Title
    override this.OnClose() = ()