namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Charts
open Prelude.Backbeat
open Prelude
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Prelude.Data.``osu!``
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Play

type ChartContextMenu(cc: CachedChart, context: LibraryContext) =
    inherit Page()

    override this.Content() =
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = pretty_pos(0, PAGE_BOTTOM, PageWidth.Normal).Translate(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
            |+ PageButton(
                %"chart.add_to_collection",
                (fun () -> AddToCollectionPage(cc).Show()),
                Icon = Icons.FOLDER_PLUS
            )
            |+ PageButton(%"chart.delete", (fun () -> ChartContextMenu.ConfirmDelete(cc, true)), Icon = Icons.TRASH)

        match context with
        | LibraryContext.None
        | LibraryContext.Table _ -> ()
        | LibraryContext.Folder name ->
            content
            |* PageButton(
                [ name ] %> "chart.remove_from_collection",
                (fun () ->
                    if CollectionActions.remove_from (name, Content.Collections.Get(name).Value, cc, context) then
                        Menu.Back()
                ),
                Icon = Icons.FOLDER_MINUS
            )
        | LibraryContext.Playlist(index, name, _) ->
            content
            |+ PageButton(
                %"chart.move_up_in_playlist",
                (fun () ->
                    if CollectionActions.reorder_up context then
                        Menu.Back()
                ),
                Icon = Icons.ARROW_UP_CIRCLE,
                Enabled = (index > 0)
            )
            |+ PageButton(
                %"chart.move_down_in_playlist",
                (fun () ->
                    if CollectionActions.reorder_down context then
                        Menu.Back()
                ),
                Icon = Icons.ARROW_DOWN_CIRCLE,
                Enabled = (index + 1 < Content.Collections.GetPlaylist(name).Value.Charts.Count)
            )
            |+ PageButton(
                [ name ] %> "chart.remove_from_collection",
                (fun () ->
                    if CollectionActions.remove_from (name, Content.Collections.Get(name).Value, cc, context) then
                        Menu.Back()
                ),
                Icon = Icons.FOLDER_MINUS
            )
            |+ PageButton.Once(
                %"playlist.play",
                (fun () ->
                    Suggestions.begin_endless_mode (
                        EndlessModeState.create_from_playlist
                            0
                            (Content.Collections.GetPlaylist(name).Value)
                            Content.Library
                    ) true
                ),
                Icon = Icons.PLAY
            )
            |+ if index > 0 then 
                [PageButton.Once(
                    %"playlist.play_from_here",
                    (fun () ->
                        Suggestions.begin_endless_mode (
                            EndlessModeState.create_from_playlist
                                index
                                (Content.Collections.GetPlaylist(name).Value)
                                Content.Library
                        ) true
                    ),
                    Icon = Icons.PLAY
                )] else []
            |* PageButton.Once(
                %"playlist.play_shuffled",
                (fun () ->
                    Suggestions.begin_endless_mode (
                        EndlessModeState.create_from_playlist_shuffled
                            (Content.Collections.GetPlaylist(name).Value)
                            Content.Library
                    ) true
                ),
                Icon = Icons.SHUFFLE
            )

        if Some cc = SelectedChart.CACHE_DATA then
            content
            |+ PageButton.Once(
                %"chart.practice",
                (fun () -> 
                    Menu.Exit()
                    SelectedChart.when_loaded(fun info ->
                        Screen.change_new
                            (fun () -> PracticeScreen.practice_screen (info, 0.0f<ms>))
                            Screen.Type.Practice
                            Transitions.Default
                        |> ignore
                    )
                ),
                Icon = Icons.TARGET
            )
            |* PageButton.Once(
                %"chart.export_osz",
                (fun () ->
                    match SelectedChart.CHART with
                    | None -> ()
                    | Some c ->
                        match create_osz c Content.Cache (get_game_folder "Exports") with
                        | Ok () ->
                            open_directory (get_game_folder "Exports")
                            Notifications.action_feedback(Icons.CHECK, %"notification.song_exported.title", "")
                        | Error err ->
                            Notifications.error(%"notification.song_export_failed.title", %"notification.song_export_failed.body")
                            Logging.Error(sprintf "Error exporting '%s' as osz" c.Header.Title, err)
                ),
                Icon = Icons.UPLOAD
            )

        match Content.Table, SelectedChart.CHART with
        | Some table, Some chart ->
            if
                Network.status = Network.Status.LoggedIn
                && cc.Keys = table.Info.Keymode
                && Chart.hash chart = cc.Hash
            then
                content
                |* PageButton(
                    %"chart.suggest_for_table",
                    (fun () -> SuggestChartPage(table, cc.Hash).Show()),
                    Icon = Icons.SIDEBAR
                )
        | _ -> ()

        content

    override this.Title = cc.Title
    override this.OnClose() = ()

    static member ConfirmDelete(cc, is_submenu) =
        let chart_name = sprintf "%s [%s]" cc.Title cc.DifficultyName

        ConfirmPage(
            [ chart_name ] %> "misc.confirmdelete",
            fun () ->
                Cache.delete cc Content.Cache
                LevelSelect.refresh_all ()

                if is_submenu then
                    Menu.Back()
        )
            .Show()