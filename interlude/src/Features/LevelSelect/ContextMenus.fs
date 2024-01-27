namespace Interlude.Features.LevelSelect

open Percyqaz.Flux.UI
open Prelude.Charts
open Prelude.Data.Scores
open Prelude.Data.Charts
open Prelude.Data.Charts.Tables
open Prelude.Data.Charts.Caching
open Prelude.Data.Charts.Collections
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.LevelSelect.Tables
open Interlude.Web.Shared.Requests

type ChartContextMenu(cc: CachedChart, context: LibraryContext) as this =
    inherit Page()

    do
        let content =
            FlowContainer.Vertical(PRETTYHEIGHT, Position = Position.Margin(100.0f, 200.0f))
            |+ PageButton(
                "chart.add_to_collection",
                (fun () ->
                    SelectCollectionPage(
                        fun (name, collection) ->
                            if CollectionActions.add_to (name, collection, cc) then
                                Menu.Back()
                        ,
                        fun (_, collection) ->
                            match collection with
                            | Folder f -> f.Contains cc
                            | Playlist p -> false
                        , true
                    )
                        .Show()
                ),
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
                    if CollectionActions.remove_from (name, Library.collections.Get(name).Value, cc, context) then
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
                Enabled = (index + 1 < Library.collections.GetPlaylist(name).Value.Charts.Count)
            )
            |* PageButton(
                "chart.remove_from_collection",
                (fun () ->
                    if CollectionActions.remove_from (name, Library.collections.Get(name).Value, cc, context) then
                        Menu.Back()
                ),
                Icon = Icons.FOLDER_MINUS,
                Text = [ name ] %> "chart.remove_from_collection.name"
            )

        // match Table.current () with
        // | Some table ->
        //     // todo: this code is wrong. you can have a context menu open for not the current chart
        //     // this should instead load the chart from disk for correctness, but the cache should ultimately store the source
        //     if Network.status = Network.Status.LoggedIn && Chart.CHART.IsSome then
        //         let chart = Chart.CHART.Value

        //         content
        //         |* PageButton(
        //             "chart.suggest_for_table",
        //             (fun () ->
        //                 SelectTableLevelPage(fun level ->
        //                     Tables.Suggestions.Add.post (
        //                         {
        //                             ChartId = cc.Hash
        //                             OsuBeatmapId =
        //                                 match chart.Header.ChartSource with
        //                                 | Osu(_, id) -> id
        //                                 | _ -> -1
        //                             EtternaPackId =
        //                                 match chart.Header.ChartSource with
        //                                 | Stepmania(pack) -> pack
        //                                 | _ -> -1
        //                             Artist = cc.Artist
        //                             Title = cc.Title
        //                             Creator = cc.Creator
        //                             Difficulty = cc.DifficultyName
        //                             TableFor = table.Name.ToLower()
        //                             SuggestedLevel = level.Rank
        //                         },
        //                         function
        //                         | Some true ->
        //                             Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
        //                         | _ -> Notifications.error ("Error sending suggestion", "")
        //                     )

        //                     Menu.Back()
        //                 )
        //                     .Show()
        //             )
        //         )
        // | None -> ()

        this.Content content

    override this.Title = cc.Title
    override this.OnClose() = ()

    static member ConfirmDelete(cc, is_submenu) =
        let chart_name = sprintf "%s [%s]" cc.Title cc.DifficultyName

        ConfirmPage(
            [ chart_name ] %> "misc.confirmdelete",
            fun () ->
                Cache.delete cc Library.cache
                LevelSelect.refresh_all ()

                if is_submenu then
                    Menu.Back()
        )
            .Show()

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
                Cache.delete_many charts Library.cache
                LevelSelect.refresh_all ()

                if is_submenu then
                    Menu.Back()
        )
            .Show()

    static member Show(name, charts, context) =
        match context with
        | LibraryGroupContext.None -> GroupContextMenu(name, charts, context).Show()
        | LibraryGroupContext.Folder id -> EditFolderPage(id, Library.collections.GetFolder(id).Value).Show()
        | LibraryGroupContext.Playlist id -> EditPlaylistPage(id, Library.collections.GetPlaylist(id).Value).Show()
        | LibraryGroupContext.Table lvl -> ()

type ScoreContextMenu(score_info: ScoreInfoProvider) as this =
    inherit Page()

    do
        this.Content(
            column ()
            |+ PageButton(
                "score.delete",
                (fun () -> ScoreContextMenu.ConfirmDeleteScore(score_info, true)),
                Icon = Icons.TRASH
            )
                .Pos(200.0f)
            |+ PageButton(
                "score.watch_replay",
                (fun () ->
                    ScoreScreenHelpers.watch_replay (score_info.Chart, score_info.ScoreInfo, score_info.ColoredChart, score_info.ReplayData)
                    Menu.Back()
                ),
                Icon = Icons.FILM
            )
                .Pos(270.0f)
            |+ PageButton(
                "score.challenge",
                (fun () ->
                    LevelSelect.challenge_score (score_info.ScoreInfo.rate, score_info.ScoreInfo.selectedMods, score_info.ReplayData)
                    Menu.Back()
                ),
                Icon = Icons.FLAG,
                Enabled = Network.lobby.IsNone
            )
                .Pos(340.0f)
                .Tooltip(Tooltip.Info("score.challenge"))
        )

    override this.Title =
        sprintf "%s | %s" (score_info.Scoring.FormatAccuracy()) (score_info.Ruleset.LampName score_info.Lamp)

    override this.OnClose() = ()

    static member ConfirmDeleteScore(score, is_submenu) =
        let score_name =
            sprintf "%s | %s" (score.Scoring.FormatAccuracy()) (score.Ruleset.LampName score.Lamp)

        ConfirmPage(
            [ score_name ] %> "misc.confirmdelete",
            fun () ->
                Chart.SAVE_DATA.Value.Scores.Remove score.ScoreInfo |> ignore
                LevelSelect.refresh_all ()
                Notifications.action_feedback (Icons.TRASH, [ score_name ] %> "notification.deleted", "")

                if is_submenu then
                    Menu.Back()
        )
            .Show()
