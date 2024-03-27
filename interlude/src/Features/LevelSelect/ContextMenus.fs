namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Charts
open Prelude.Backbeat
open Prelude.Data
open Prelude.Data.Library
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Interlude.Utils
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.Collections
open Interlude.Features.Tables

type ChartContextMenu(cc: CachedChart, context: LibraryContext) as this =
    inherit Page()

    do
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))
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
                            false
                            (Content.Collections.GetPlaylist(name).Value)
                            Content.Library
                    )

                    Endless.continue_endless_mode (fun info -> LevelSelect.try_play info) |> ignore
                ),
                Icon = Icons.PLAY
            )
            |* PageButton.Once(
                "playlist.play_shuffled",
                (fun () ->
                    Endless.begin_endless_mode (
                        EndlessModeState.create_from_playlist
                            true
                            (Content.Collections.GetPlaylist(name).Value)
                            Content.Library
                    )

                    Endless.continue_endless_mode (fun info -> LevelSelect.try_play info) |> ignore
                ),
                Icon = Icons.SHUFFLE
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

type PlaylistContextMenu(name: string, playlist: Playlist) =
    inherit Page()

    override this.Init(parent) =
        page_container()
        |+ PageButton("collections.edit", (fun () -> EditPlaylistPage(name, playlist).Show()), Icon = Icons.EDIT_2)
            .Pos(0)
        |+ PageButton
            .Once(
                "playlist.play",
                (fun () ->
                    Endless.begin_endless_mode (EndlessModeState.create_from_playlist false playlist Content.Library)
                    Endless.continue_endless_mode (fun info -> LevelSelect.try_play info) |> ignore
                ),
                Icon = Icons.PLAY
            )
            .Pos(3)
        |+ PageButton
            .Once(
                "playlist.play_shuffled",
                (fun () ->
                    Endless.begin_endless_mode (EndlessModeState.create_from_playlist true playlist Content.Library)
                    Endless.continue_endless_mode (fun info -> LevelSelect.try_play info) |> ignore
                ),
                Icon = Icons.SHUFFLE
            )
            .Pos(5)
        |> this.Content

        base.Init parent

    override this.Title = name
    override this.OnClose() = ()

type GroupContextMenu(name: string, charts: CachedChart seq, context: LibraryGroupContext) as this =
    inherit Page()

    do
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))
            |+ PageButton(
                "group.delete",
                (fun () -> GroupContextMenu.ConfirmDelete(name, charts, true)),
                Icon = Icons.TRASH
            )
                .Tooltip(Tooltip.Info("group.delete"))

        this.Content content

    override this.Title = name
    override this.OnClose() = ()

    static member ConfirmDelete(name, charts, is_submenu) =
        let group_name = sprintf "%s (%i charts)" name (Seq.length charts)

        ConfirmPage(
            [ group_name ] %> "misc.confirmdelete",
            fun () ->
                Cache.delete_many charts Content.Cache
                LevelSelect.refresh_all ()

                if is_submenu then
                    Menu.Back()
        )
            .Show()

    static member Show(name, charts, context) =
        match context with
        | LibraryGroupContext.None -> GroupContextMenu(name, charts, context).Show()
        | LibraryGroupContext.Folder id -> EditFolderPage(id, Content.Collections.GetFolder(id).Value).Show()
        | LibraryGroupContext.Playlist id -> PlaylistContextMenu(id, Content.Collections.GetPlaylist(id).Value).Show()
        | LibraryGroupContext.Table lvl -> ()

type ScoreContextMenu(score_info: ScoreInfo) as this =
    inherit Page()

    do
        this.Content(
            page_container()
            |+ PageButton(
                "score.delete",
                (fun () -> ScoreContextMenu.ConfirmDeleteScore(score_info, true)),
                Icon = Icons.TRASH
            )
                .Pos(0)
            |+ PageButton(
                "score.watch_replay",
                (fun () ->
                    ScoreScreenHelpers.watch_replay (score_info, Chart.color_this_chart (score_info.WithMods))
                    Menu.Back()
                ),
                Icon = Icons.FILM
            )
                .Pos(2)
            |+ PageButton(
                "score.challenge",
                (fun () ->
                    LevelSelect.challenge_score score_info
                    Menu.Back()
                ),
                Icon = Icons.FLAG,
                Enabled = Network.lobby.IsNone
            )
                .Pos(4)
                .Tooltip(Tooltip.Info("score.challenge"))
        )

    override this.Title =
        sprintf "%s | %s" (score_info.Scoring.FormatAccuracy()) (score_info.Ruleset.LampName score_info.Lamp)

    override this.OnClose() = ()

    static member ConfirmDeleteScore(score_info, is_submenu) =
        let score_name =
            sprintf "%s | %s" (score_info.Scoring.FormatAccuracy()) (score_info.Ruleset.LampName score_info.Lamp)

        ConfirmPage(
            [ score_name ] %> "misc.confirmdelete",
            fun () ->
                if ScoreDatabase.delete_score score_info.CachedChart.Hash score_info.TimePlayed Content.Scores then
                    LevelSelect.refresh_all ()
                    Notifications.action_feedback (Icons.TRASH, [ score_name ] %> "notification.deleted", "")
                else
                    Logging.Debug("Couldn't find score matching timestamp to delete")

                if is_submenu then
                    Menu.Back()
        )
            .Show()
