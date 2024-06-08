namespace Interlude.Features.Tables

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Backbeat
open Prelude.Backbeat.Archive
open Prelude.Data.Library.Caching
open Prelude
open Interlude.Content
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Import
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
                        Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
                    | Some Tables.Suggestions.Vote.Response.OkDetailsRequired ->
                        // todo: send backbeat addition request with suggestion
                        Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
                    | Some Tables.Suggestions.Vote.Response.Rejected ->
                        Notifications.action_feedback (
                            Icons.X_CIRCLE,
                            "Suggestion rejected!",
                            "This chart has already previously been rejected"
                        )
                    | None -> Notifications.error ("Error sending suggestion", "")
                )

                Menu.Back()
        )

type Suggestion =
    {
        ChartId: string
        Votes: Map<int, int>
        BackbeatInfo: (Chart * Song) option
        LocalChart: CachedChart option
    }
    member this.SongTitle =
        match this.BackbeatInfo with
        | Some (_, song) -> song.Title
        | None ->

        match this.LocalChart with
        | Some cc -> cc.Title
        | None ->

        "???"
    member this.FormattedTitle =
        match this.BackbeatInfo with
        | Some (_, song) -> song.FormattedTitle
        | None ->

        match this.LocalChart with
        | Some cc -> cc.Artist + " - " + cc.Title
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
                Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion rejected!", "")
                Menu.Back(); Menu.Back()
            | _ -> Notifications.error ("Error rejecting suggestion", "")
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
                Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion applied!", "")
            | _ -> Notifications.error ("Error applying suggestion", "")
        )
        Menu.Back()

    let change_vote (level: int) =
        Tables.Suggestions.Vote.post (
            {
                ChartId = suggestion.ChartId
                TableId = table.Id
                Level = level
            },
            function
            | Some Tables.Suggestions.Vote.Response.Ok ->
                Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
            | Some Tables.Suggestions.Vote.Response.OkDetailsRequired ->
                // todo: send backbeat addition request with suggestion
                Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
            | Some Tables.Suggestions.Vote.Response.Rejected ->
                Notifications.action_feedback (
                    Icons.X_CIRCLE,
                    "Suggestion rejected!",
                    "This chart has already previously been rejected"
                )
            | None -> Notifications.error ("Error sending suggestion", "")
        )

        Menu.Back()
        

    let mutable still_open = true
    let playtest_suggestion () =
        match suggestion.LocalChart with
        | Some cc ->
            SelectedChart.change(cc, Data.Library.Collections.LibraryContext.None, true)
            Menu.Exit()
        | None ->

        download_chart_by_hash.Request(
            (suggestion.ChartId, table.Id),
            function
            | true -> 
                Notifications.task_feedback(Icons.DOWNLOAD, %"notification.install_song", "")
                defer (fun () ->
                    if still_open then
                        match Cache.by_hash suggestion.ChartId Content.Cache with
                        | Some cc ->
                            SelectedChart.change(cc, Data.Library.Collections.LibraryContext.None, true)
                            Menu.Exit()
                        | None -> ()
                )
            | false ->
                Notifications.error(%"notification.install_song_failed", "")
        )

    override this.Content() =
        page_container()
        |+ PageButton.Once(%"table.suggestions.playtest", playtest_suggestion, Icon = Icons.PLAY, Enabled = (suggestion.LocalChart.IsSome || suggestion.BackbeatInfo.IsSome))
            .Pos(6)
        |+ PageButton(
            %"table.suggestions.vote_another_level", 
            (fun () -> SuggestChartPage(table, suggestion.ChartId).Show()),
            Icon = Icons.EDIT_2
        )
            .Pos(8)
        |+ PageButton(%"table.suggestions.accept", (fun () -> SelectTableLevelPage(table, accept_level).Show()), Icon = Icons.CHECK, Enabled = suggestion.BackbeatInfo.IsSome)
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
                yield Text(server_song.FormattedTitle, Align = Alignment.LEFT, Position = pretty_pos(0, 2, PageWidth.Full))
                yield Text(server_chart.DifficultyName + "  •  " + server_chart.FormattedCreators, Align = Alignment.LEFT, Color = K Colors.text_subheading, Position = pretty_pos(2, 1, PageWidth.Full))
                yield Text(vote_text, Align = Alignment.LEFT, Color = K Colors.text_subheading, Position = pretty_pos(3, 1, PageWidth.Full))
            | None ->

            match suggestion.LocalChart with
            | Some local_cc ->
                yield Text(local_cc.Artist + " - " + local_cc.Title, Align = Alignment.LEFT, Position = pretty_pos(0, 2, PageWidth.Full))
                yield Text(local_cc.DifficultyName + "  •  " + local_cc.Creator, Align = Alignment.LEFT, Color = K Colors.text_subheading, Position = pretty_pos(2, 1, PageWidth.Full))
                yield Text(vote_text, Align = Alignment.LEFT, Color = K Colors.text_subheading, Position = pretty_pos(3, 1, PageWidth.Full))
            | None ->
                yield Text(%"table.suggestions.info_missing", Align = Alignment.LEFT, Color = K Colors.text_red_2, Position = pretty_pos(0, 2, PageWidth.Full))
                yield Text(vote_text, Align = Alignment.LEFT, Color = K Colors.text_subheading, Position = pretty_pos(2, 1, PageWidth.Full))
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
                            defer
                            <| fun () ->
                                match response with
                                | Some result -> this.SetData result
                                | None -> this.ServerError()
                    )
                else
                    this.Offline()
            , fun _ data ->
                let fc = FlowContainer.Vertical<PageButton>(PRETTYHEIGHT, Spacing = 5.0f)

                for server_suggestion in data.Suggestions do
                    let suggestion =
                        {
                            ChartId = server_suggestion.ChartId
                            Votes = server_suggestion.Votes
                            BackbeatInfo = server_suggestion.BackbeatInfo
                            LocalChart = Cache.by_hash server_suggestion.ChartId Interlude.Content.Content.Cache
                        }
                    fc.Add(PageButton(suggestion.FormattedTitle, fun () -> ViewSuggestionPage(table, suggestion).Show()))

                defer (fun () -> fc.Focus false)

                ScrollContainer(fc, Position = Position.Margin(100.0f, 200.0f), Margin = 5.0f)
        )

type SuggestionsPage(table: Table) =
    inherit Page()

    let suggestions_list = SuggestionsList(table)

    override this.Content() =
        NavigationContainer.Column<Widget>()
        |+ Button("", ignore, Position = Position.Box(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 0.0f))
        |+ suggestions_list 
        |>> Container
        :> Widget

    override this.Title = %"table.suggestions"
    override this.OnClose() = ()
    override this.OnReturnTo() = suggestions_list.Reload()
