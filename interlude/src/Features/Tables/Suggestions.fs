namespace Interlude.Features.Tables

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Backbeat
open Prelude.Data.Library.Caching
open Prelude
open Interlude.UI
open Interlude.UI.Menu
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

// todo: redo this whole thing keeping it simple instead of trying to design a whole fancy UI for it
(*
top level page:
 - "suggest this chart" button
 - list of suggestions by chart title and vote count
click a suggestion:
 - details about the chart
 - vote breakdowns by level (clickable to confirm)
 - button to suggest another level
 - button to playtest
*)

type Suggestion(table: Table, suggestion: Tables.Suggestions.List.Suggestion) =
    inherit FrameContainer(NodeType.Leaf)

    let mutable height = 100.0f

    // todo: tooltip on hover
    let button (icon: string, action) =
        { new Button(icon, action, Position = Position.TrimRight(20.0f).SliceRight(60.0f)) with
            override this.Draw() =
                Draw.rect this.Bounds Colors.black.O2
                base.Draw()
        }

    let vote_row (level: int) =
        let container =
            Container(NodeType.None, Position = Position.Row(height, 40.0f))
            |+ Text(
                sprintf "Level %02i - %i Votes" level (suggestion.Votes.[level]),
                Position = Position.Margin(10.0f, 0.0f),
                Align = Alignment.LEFT
            )

        match suggestion.BackbeatInfo with
        | Some(chart, song) ->
            container
            |* button (
                Icons.CHECK,
                fun () ->
                    ConfirmPage(
                        sprintf "Add %s to level %i?" song.Title level,
                        fun () ->
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
                    )
                        .Show()
            )
        | None -> ()

        container

    let actions =
        let fc =
            FlowContainer.RightToLeft(60.0f, Spacing = 20.0f, Position = Position.SliceTop(40.0f).TrimRight(20.0f))

        // todo: playtesting button
        // todo: delete button

        fc.Add(
            button (
                Icons.EDIT_2,
                fun () ->
                    SelectTableLevelPage(
                        table,
                        fun level ->
                            Tables.Suggestions.Vote.post (
                                ({
                                    ChartId = suggestion.ChartId
                                    TableId = table.Id
                                    Level = level
                                }
                                : Tables.Suggestions.Vote.Request),
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
                        .Show()
            )
        )

        fc

    override this.Init(parent: Widget) =
        this
        |+ Text(
            match suggestion.BackbeatInfo with
            | Some(chart, song) -> song.Title
            | None -> "???"
            , Position = Position.Row(0.0f, 40.0f).Margin(10.0f, 0.0f)
            , Align = Alignment.LEFT
        )
        |+ Text(
            match suggestion.BackbeatInfo with
            | Some(chart, song) -> sprintf "%s  •  %s" song.FormattedArtists chart.FormattedCreators
            | None -> "Metadata missing"
            , Position = Position.Row(40.0f, 30.0f).Margin(10.0f, 0.0f)
            , Align = Alignment.LEFT
        )
        |+ Text(
            match suggestion.BackbeatInfo with
            | Some(chart, song) -> chart.DifficultyName
            | None -> "???"
            , Position = Position.Row(70.0f, 30.0f).Margin(10.0f, 0.0f)
            , Align = Alignment.LEFT
            , Color = K Colors.text_greyout
        )
        |* actions

        for level in suggestion.Votes.Keys do
            this |* vote_row level
            height <- height + 40.0f

        base.Init parent

    interface IHeight with
        member this.Height = height

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
                let fc = DynamicFlowContainer.Vertical<Suggestion>(Spacing = 30.0f)

                for s in data.Suggestions do
                    fc.Add(Suggestion(table, s))

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

type SuggestChartPage(table: Table, cc: CachedChart) =
    inherit
        SelectTableLevelPage(
            table,
            fun level ->
                Tables.Suggestions.Vote.post (
                    {
                        ChartId = cc.Hash
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
