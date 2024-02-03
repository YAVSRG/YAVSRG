namespace Interlude.Features.LevelSelect.Tables

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Prelude.Data.Charts.Tables
open Interlude.Utils
open Interlude.UI
open Interlude.UI.Menu
open Interlude.UI.Components
open Interlude.Features.Online
open Interlude.Web.Shared.Requests

type Suggestion(table: Table, suggestion: Tables.Suggestions.List.Suggestion) =
    inherit Frame(NodeType.Leaf)

    let mutable size = 100.0f

    // todo: tooltip on hover
    let button (icon: string, action) =
        { new Button(icon, action, Position = Position.TrimRight(20.0f).SliceRight(60.0f)) with
            override this.Draw() =
                Draw.rect this.Bounds Colors.black.O2
                base.Draw()
        }

    let vote_row (level: int) =
        let container =
            StaticContainer(NodeType.None, Position = Position.Row(size, 40.0f))
            |+ Text(
                sprintf "Level %02i - %i Votes" level (suggestion.Votes.[level]),
                Position = Position.Margin(10.0f, 0.0f),
                Align = Alignment.LEFT
            )

        match suggestion.BackbeatInfo with
        | Some (chart, song) ->
            container
            |* button (
                Icons.CHECK,
                fun () ->
                    ConfirmPage(
                        sprintf "Add %s to level %i?" song.Title level,
                        fun () ->
                            Tables.Suggestions.Accept.post (
                                ({ TableId = table.Id; ChartId = suggestion.ChartId; Level = level }: Tables.Suggestions.Accept.Request),
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
                    SelectTableLevelPage(table, fun level ->
                        Tables.Suggestions.Vote.post (
                            ({
                                ChartId = suggestion.ChartId
                                TableId = table.Id
                                Level = level
                            }
                            : Tables.Suggestions.Vote.Request),
                            function
                            | Some Tables.Suggestions.Vote.Response.Ok -> Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
                            | Some Tables.Suggestions.Vote.Response.OkDetailsRequired -> 
                                // todo: send backbeat addition request with suggestion
                                Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
                            | Some Tables.Suggestions.Vote.Response.Rejected -> Notifications.action_feedback (Icons.X_CIRCLE, "Suggestion rejected!", "This chart has already previously been rejected")
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
            | Some (chart, song) -> song.Title 
            | None -> "???"
            , 
            Position = Position.Row(0.0f, 40.0f).Margin(10.0f, 0.0f), Align = Alignment.LEFT)
        |+ Text(
            match suggestion.BackbeatInfo with
            | Some (chart, song) -> sprintf "%s  •  %s" song.FormattedArtists chart.FormattedCreators
            | None -> "Metadata missing"
            , 
            Position = Position.Row(40.0f, 30.0f).Margin(10.0f, 0.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            match suggestion.BackbeatInfo with
            | Some (chart, song) -> chart.DifficultyName
            | None -> "???"
            , 
            Position = Position.Row(70.0f, 30.0f).Margin(10.0f, 0.0f),
            Align = Alignment.LEFT,
            Color = K Colors.text_greyout
        )
        |* actions

        for level in suggestion.Votes.Keys do
            this |* vote_row level
            size <- size + 40.0f

        base.Init parent

    interface DynamicSize with
        member this.Size = size
        member this.OnSizeChanged with set _ = ()

type SuggestionsList(table: Table) =
    inherit
        WebRequestContainer<Tables.Suggestions.List.Response>(
            fun this ->
                if Network.status = Network.Status.LoggedIn then
                    Tables.Suggestions.List.get (
                        table.Id,
                        fun response ->
                            sync
                            <| fun () ->
                                match response with
                                | Some result -> this.SetData result
                                | None -> this.ServerError()
                    )
                else this.Offline()
            , fun data ->
                let fc = DynamicFlowContainer.Vertical<Suggestion>(Spacing = 30.0f)

                for s in data.Suggestions do
                    fc.Add(Suggestion(table, s))

                sync (fun () -> fc.Focus())

                ScrollContainer(fc, Position = Position.Margin(100.0f, 200.0f), Margin = 5.0f)
        )

type SuggestionsPage(table: Table) as this =
    inherit Page()

    let sl = SuggestionsList(table)

    do this.Content(StaticContainer(NodeType.Leaf) |+ sl)

    override this.Title = %"table.suggestions.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = sl.Reload()
