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

//type Suggestion(suggestion: Tables.Suggestions.List.Suggestion) =
//    inherit Frame(NodeType.Leaf)

//    let mutable size = 100.0f

//    // todo: tooltip on hover
//    let button (icon: string, action) =
//        { new Button(icon, action, Position = Position.TrimRight(20.0f).SliceRight(60.0f)) with
//            override this.Draw() =
//                Draw.rect this.Bounds Colors.black.O2
//                base.Draw()
//        }

//    let suggested_row (level: int) =
//        let container =
//            StaticContainer(NodeType.None, Position = Position.Row(size, 40.0f))
//            |+ Text(
//                sprintf "Level %i; Suggested by %s" level (suggestion.LevelsSuggestedBy.[level] |> String.concat ", "),
//                Position = Position.Margin(10.0f, 0.0f),
//                Align = Alignment.LEFT
//            )

//        if suggestion.CanApply then
//            container
//            |* button (
//                Icons.CHECK,
//                fun () ->
//                    ConfirmPage(
//                        sprintf "Add %s to level %i?" suggestion.Title level,
//                        fun () ->
//                            Tables.Suggestions.Apply.post (
//                                ({ Id = suggestion.Id; Level = level }: Tables.Suggestions.Apply.Request),
//                                function
//                                | Some true ->
//                                    Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion applied!", "")
//                                | _ -> Notifications.error ("Error applying suggestion", "")
//                            )
//                    )
//                        .Show()
//            )

//        container

//    let actions =
//        let fc =
//            FlowContainer.RightToLeft(60.0f, Spacing = 20.0f, Position = Position.SliceTop(40.0f).TrimRight(20.0f))

//        // todo: playtesting button
//        // todo: delete button

//        fc.Add(
//            button (
//                Icons.EDIT_2,
//                fun () ->
//                    SelectTableLevelPage(fun level ->
//                        Tables.Suggestions.Add.post (
//                            ({
//                                ChartId = suggestion.ChartId
//                                OsuBeatmapId = suggestion.OsuBeatmapId
//                                EtternaPackId = suggestion.EtternaPackId
//                                Artist = suggestion.Artist
//                                Title = suggestion.Title
//                                Creator = suggestion.Creator
//                                Difficulty = suggestion.Difficulty
//                                TableFor = Table.current().Value.Name.ToLower()
//                                SuggestedLevel = level.Rank
//                            }
//                            : Tables.Suggestions.Add.Request),
//                            function
//                            | Some true -> Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
//                            | _ -> Notifications.error ("Error sending suggestion", "")
//                        )

//                        Menu.Back()
//                    )
//                        .Show()
//            )
//        )

//        fc

//    override this.Init(parent: Widget) =
//        this
//        |+ Text(suggestion.Title, Position = Position.Row(0.0f, 40.0f).Margin(10.0f, 0.0f), Align = Alignment.LEFT)
//        |+ Text(
//            sprintf "%s  •  %s" suggestion.Artist suggestion.Creator,
//            Position = Position.Row(40.0f, 30.0f).Margin(10.0f, 0.0f),
//            Align = Alignment.LEFT
//        )
//        |+ Text(
//            suggestion.Difficulty,
//            Position = Position.Row(70.0f, 30.0f).Margin(10.0f, 0.0f),
//            Align = Alignment.LEFT,
//            Color = K Colors.text_greyout
//        )
//        |* actions

//        for level in suggestion.LevelsSuggestedBy.Keys do
//            this |* suggested_row level
//            size <- size + 40.0f

//        base.Init parent

//    interface FlowContainerV2.FlowContainerItem with
//        member this.Size = size
//        member this.OnSizeChanged _ = ()

//type SuggestionsList() =
//    inherit
//        WebRequestContainer<Tables.Suggestions.List.Response>(
//            fun this ->
//                if Network.status = Network.Status.LoggedIn then
//                    match Table.current () with
//                    | Some table ->
//                        Tables.Suggestions.List.get (
//                            table.Name.ToLower(),
//                            fun response ->
//                                sync
//                                <| fun () ->
//                                    match response with
//                                    | Some result -> this.SetData result
//                                    | None -> this.ServerError()
//                        )
//                    | None -> failwith "impossible"
//                else
//                    this.Offline()
//            , fun data ->
//                let fc = FlowContainerV2.Vertical<Suggestion>(Spacing = 30.0f)

//                for s in data.Suggestions do
//                    fc.Add(Suggestion(s))

//                sync (fun () -> fc.Focus())

//                ScrollContainer.Flow(fc, Position = Position.Margin(100.0f, 200.0f), Margin = 5.0f)
//        )

type SuggestionsPage() as this =
    inherit Page()

    let sl = Dummy()//SuggestionsList()

    do this.Content(StaticContainer(NodeType.Leaf) |+ sl)

    override this.Title = %"table.suggestions.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = ()//sl.Reload()
