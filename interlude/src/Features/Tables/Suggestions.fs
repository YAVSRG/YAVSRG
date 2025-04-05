namespace Interlude.Features.Tables

open Percyqaz.Common
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude.Backbeat
open Prelude.Backbeat.Archive
open Prelude.Data.Library
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

type SuggestChartPage(table: Table, chart_id: string) =
    inherit
        SelectTableLevelPage(
            table,
            fun level ->
                Tables.Suggestions.Vote.post (
                    {
                        ChartId = chart_id
                        TableId = table.Id
                        Level = level
                    },
                    function
                    | Some Tables.Suggestions.Vote.Response.Ok ->
                        Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.suggestion.send_success.title", "")
                    | Some Tables.Suggestions.Vote.Response.OkDetailsRequired ->
                        // todo: send backbeat addition request with suggestion
                        Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.suggestion.send_success.title", "")
                    | Some Tables.Suggestions.Vote.Response.Rejected ->
                        Notifications.action_feedback (
                            Icons.X_CIRCLE,
                            %"notification.suggestion.rejected.title",
                            %"notification.suggestion.rejected.body"
                        )
                    | None -> Notifications.error (%"notification.suggestion.server_error.title", "")
                )

                Menu.Back()
        )

type Suggestion =
    {
        ChartId: string
        Votes: Map<int, int>
        BackbeatInfo: (Chart * Song) option
        LocalChart: ChartMeta option
    }
    member this.SongTitle =
        match this.BackbeatInfo with
        | Some (_, song) -> song.Title
        | None ->

        match this.LocalChart with
        | Some chart_meta -> chart_meta.Title
        | None ->

        "???"
    member this.FormattedTitle =
        match this.BackbeatInfo with
        | Some (_, song) -> song.FormattedTitle
        | None ->

        match this.LocalChart with
        | Some chart_meta -> chart_meta.Artist + " - " + chart_meta.Title
        | None ->

        "???"

type RejectSuggestionPage(table: Table, suggestion: Suggestion) =
    inherit Page()

    let reason = Setting.simple ""

    let reject() =
        Tables.Suggestions.Reject.post (
            ({
                TableId = table.Id
                ChartId = suggestion.ChartId
                Reason = reason.Value
            }
            : Tables.Suggestions.Reject.Request),
            function
            | Some true ->
                Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.suggestion.rejected.title", "")
                Menu.Back(); Menu.Back()
            | _ -> Notifications.error (%"notification.suggestion.server_error.title", "")
        )

    override this.Content() =
        page_container()
        |+ PageTextEntry(%"table.suggestions.reject_reason", reason).Pos(0)
        |+ PageButton.Once(%"table.suggestions.reject", reject).Pos(3)
        :> Widget

    override this.Title = suggestion.SongTitle
    override this.OnClose() = ()

type ViewSuggestionPage(table: Table, suggestion: Suggestion) =
    inherit Page()

    let accept_level (level: int) =
        Tables.Suggestions.Accept.post (
            ({
                TableId = table.Id
                ChartId = suggestion.ChartId
                Level = level
            }
            : Tables.Suggestions.Accept.Request),
            function
            | Some true ->
                Notifications.action_feedback (Icons.FOLDER_PLUS, %"notification.suggestion.apply_success.title", "")
            | _ -> Notifications.error (%"notification.suggestion.server_error.title", "")
        )
        Menu.Back()

    let mutable still_open = true
    let playtest_suggestion () =
        match suggestion.LocalChart with
        | Some chart_meta ->
            SelectedChart.change(chart_meta, LibraryContext.None, true)
            Menu.Exit()
        | None ->

        Backbeat.download_missing_chart.Request(
            (suggestion.ChartId, table.Id),
            function
            | true ->
                Notifications.task_feedback(Icons.DOWNLOAD, %"notification.install_song", "")
                GameThread.defer (fun () ->
                    if still_open then
                        match ChartDatabase.get_meta suggestion.ChartId Content.Charts with
                        | Some chart_meta ->
                            SelectedChart.change(chart_meta, LibraryContext.None, true)
                            Menu.Exit()
                        | None -> ()
                )
            | false ->
                Notifications.error(%"notification.install_song_failed", "")
        )

    override this.Content() =
        page_container()
        |+ PageButton.Once(%"table.suggestions.playtest", playtest_suggestion, K (suggestion.LocalChart.IsNone && suggestion.BackbeatInfo.IsNone), Icon = Icons.PLAY)
            .Pos(6)
        |+ PageButton(
            %"table.suggestions.vote_another_level",
            (fun () -> SuggestChartPage(table, suggestion.ChartId).Show()),
            Icon = Icons.EDIT_2
        )
            .Pos(8)
        |+ PageButton(%"table.suggestions.accept", (fun () -> SelectTableLevelPage(table, accept_level).Show()), Icon = Icons.CHECK, Disabled = K suggestion.BackbeatInfo.IsNone)
            .Conditional(fun () -> Network.credentials.Username = "Percyqaz") // todo: add permission check instead of temporary check
            .Pos(10)
        |+ PageButton(%"table.suggestions.reject", (fun () -> RejectSuggestionPage(table, suggestion).Show()), Icon = Icons.X)
            .Conditional(fun () -> Network.credentials.Username = "Percyqaz")
            .Pos(12)
        |+ seq {
            let vote_text =
                suggestion.Votes
                |> Map.toSeq
                |> Seq.map (fun (level, count) -> sprintf "%s (%i)" (table.Info.LevelName level) count)
                |> String.concat ", "
            match suggestion.BackbeatInfo with
            | Some (server_chart, server_song) ->
                yield Text(server_song.FormattedTitle)
                    .Align(Alignment.LEFT)
                    .Pos(0, 2, PageWidth.Full)
                yield Text(server_chart.DifficultyName + "  •  " + server_chart.FormattedCreators)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Pos(2, 1, PageWidth.Full)
                yield Text(vote_text)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Pos(3, 1, PageWidth.Full)
            | None ->

            match suggestion.LocalChart with
            | Some local_chart_meta ->
                yield Text(local_chart_meta.Artist + " - " + local_chart_meta.Title)
                    .Align(Alignment.LEFT)
                    .Pos(0, 2, PageWidth.Full)
                yield Text(local_chart_meta.DifficultyName + "  •  " + local_chart_meta.Creator)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Pos(2, 1, PageWidth.Full)
                yield Text(vote_text)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Pos(3, 1, PageWidth.Full)
            | None ->
                yield Text(%"table.suggestions.info_missing")
                    .Color(Colors.text_red_2)
                    .Align(Alignment.LEFT)
                    .Pos(0, 2, PageWidth.Full)
                yield Text(vote_text)
                    .Color(Colors.text_subheading)
                    .Align(Alignment.LEFT)
                    .Pos(2, 1, PageWidth.Full)
        } :> Widget

    override this.Title = %"table.suggestion"
    override this.OnClose() = still_open <- false

type SuggestionsList(table: Table) =
    inherit
        WebRequestContainer<Tables.Suggestions.List.Response>(
            fun this ->
                if Network.status = Network.Status.LoggedIn then
                    Tables.Suggestions.List.get (
                        table.Id,
                        fun response ->
                            GameThread.defer
                            <| fun () ->
                                match response with
                                | Some result -> this.SetData result
                                | None -> this.ServerError()
                    )
                else
                    this.Offline()
            , fun _ data ->
                let fc = FlowContainer.Vertical<PageButton>(PAGE_ITEM_HEIGHT, Spacing = 5.0f)

                for server_suggestion in data.Suggestions do
                    let suggestion =
                        {
                            ChartId = server_suggestion.ChartId
                            Votes = server_suggestion.Votes
                            BackbeatInfo = server_suggestion.BackbeatInfo
                            LocalChart = ChartDatabase.get_meta server_suggestion.ChartId Content.Charts
                        }
                    fc.Add(PageButton(suggestion.FormattedTitle, fun () -> ViewSuggestionPage(table, suggestion).Show()))

                GameThread.defer (fun () -> fc.Focus false)

                ScrollContainer(fc)
                    .Margin(Style.PADDING)
                    .Position(Position.Shrink(100.0f, 200.0f))
        )

type SuggestionsPage(table: Table) =
    inherit Page()

    let suggestions_list = SuggestionsList(table)

    override this.Content() =
        NavigationContainer.Column()
            .With(
                Dummy(NodeType.Leaf),
                suggestions_list
            )

    override this.Title = %"table.suggestions"
    override this.OnClose() = ()
    override this.OnReturnFromNestedPage() = suggestions_list.Reload()