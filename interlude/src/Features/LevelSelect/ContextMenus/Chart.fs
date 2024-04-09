namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude.Charts
open Prelude.Backbeat
open Prelude
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Collections
open Interlude.Features.Tables
open Interlude.Features.Play

type ChartContextMenu(cc: CachedChart, context: LibraryContext) as this =
    inherit Page()

    do
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = pretty_pos(0, PAGE_BOTTOM, PageWidth.Normal).Translate(PRETTY_MARGIN_X, PRETTY_MARGIN_Y))
            |+ PageButton(
                "chart.add_to_collection",
                (fun () -> AddToCollectionPage(cc).Show()),
                Icon = Icons.FOLDER_PLUS
            )
            |+ PageButton("chart.delete", (fun () -> ChartContextMenu.ConfirmDelete(cc, true)), Icon = Icons.TRASH)

        match context with
        | LibraryContext.None
        | LibraryContext.Table _ -> ()
        | LibraryContext.Folder name ->
            content
            |* PageButton(
                "chart.remove_from_collection",
                (fun () ->
                    if CollectionActions.remove_from (name, Content.Collections.Get(name).Value, cc, context) then
                        Menu.Back()
                ),
                Icon = Icons.FOLDER_MINUS,
                Text = [ name ] %> "chart.remove_from_collection.name"
            )
        | LibraryContext.Playlist(index, name, _) ->
            content
            |+ PageButton(
                "chart.move_up_in_playlist",
                (fun () ->
                    if CollectionActions.reorder_up context then
                        Menu.Back()
                ),
                Icon = Icons.ARROW_UP_CIRCLE,
                Enabled = (index > 0)
            )
            |+ PageButton(
                "chart.move_down_in_playlist",
                (fun () ->
                    if CollectionActions.reorder_down context then
                        Menu.Back()
                ),
                Icon = Icons.ARROW_DOWN_CIRCLE,
                Enabled = (index + 1 < Content.Collections.GetPlaylist(name).Value.Charts.Count)
            )
            |+ PageButton(
                "chart.remove_from_collection",
                (fun () ->
                    if CollectionActions.remove_from (name, Content.Collections.Get(name).Value, cc, context) then
                        Menu.Back()
                ),
                Icon = Icons.FOLDER_MINUS,
                Text = [ name ] %> "chart.remove_from_collection.name"
            )
            |+ PageButton.Once(
                "playlist.play",
                (fun () ->
                    Endless.begin_endless_mode (
                        EndlessModeState.create_from_playlist
                            0
                            (Content.Collections.GetPlaylist(name).Value)
                            Content.Library
                    )

                    Endless.continue_endless_mode (fun info -> LevelSelect.try_play info) |> ignore
                ),
                Icon = Icons.PLAY
            )
            |+ if index > 0 then 
                [PageButton.Once(
                    "playlist.play_from_here",
                    (fun () ->
                        Endless.begin_endless_mode (
                            EndlessModeState.create_from_playlist
                                index
                                (Content.Collections.GetPlaylist(name).Value)
                                Content.Library
                        )

                        Endless.continue_endless_mode (fun info -> LevelSelect.try_play info) |> ignore
                    ),
                    Icon = Icons.PLAY
                )] else []
            |* PageButton.Once(
                "playlist.play_shuffled",
                (fun () ->
                    Endless.begin_endless_mode (
                        EndlessModeState.create_from_playlist_shuffled
                            (Content.Collections.GetPlaylist(name).Value)
                            Content.Library
                    )

                    Endless.continue_endless_mode (fun info -> LevelSelect.try_play info) |> ignore
                ),
                Icon = Icons.SHUFFLE
            )

        if Some cc = Chart.CACHE_DATA then
            content
            |* PageButton.Once(
                "chart.practice",
                (fun () -> 
                    Menu.Exit()
                    Chart.when_loaded(fun info ->
                        Screen.change_new
                            (fun () -> PracticeScreen.practice_screen (info, 0.0f<ms>))
                            Screen.Type.Practice
                            Transitions.Flags.Default
                        |> ignore
                    )
                ),
                Icon = Icons.TARGET
            )

        match Content.Table, Chart.CHART with
        | Some table, Some chart ->
            if
                Network.status = Network.Status.LoggedIn
                && cc.Keys = table.Info.Keymode
                && Chart.hash chart = cc.Hash
            then
                content
                |* PageButton(
                    "chart.suggest_for_table",
                    (fun () -> SuggestChartPage(table, cc).Show()),
                    Icon = Icons.SIDEBAR
                )
        | _ -> ()

        this.Content content

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