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

type Suggestion(suggestion: Tables.Suggestions.List.Suggestion) as this =
    inherit Frame(NodeType.Switch (fun () -> this.Actions))

    // todo: tooltip on hover
    let button (icon: string, action) =
        { new Button(icon, action) with
            override this.Draw() =
                Draw.rect this.Bounds Colors.black.O2
                base.Draw()
        }

    let actions =
        let fc = FlowContainer.RightToLeft(60.0f, Spacing = 20.0f, Position = Position.SliceTop(40.0f).TrimRight(20.0f))
        if suggestion.CanApply then
            fc.Add(button(Icons.CHECK, 
                fun () ->
                    SelectTableLevelPage(
                        fun level ->
                            Tables.Suggestions.Apply.post(
                                ({
                                    Id = suggestion.Id
                                    Level = level.Rank
                                } : Tables.Suggestions.Apply.Request),
                                function
                                | Some true ->
                                    Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion applied!", "")
                                | _ -> Notifications.error ("Error applying suggestion", "")
                            )
                            Menu.Back()
                    ).Show()
                ))

        fc.Add(button(Icons.EDIT_2, 
            fun () ->
                SelectTableLevelPage(
                    fun level ->
                        Tables.Suggestions.Add.post(
                            ({
                                ChartId = suggestion.ChartId
                                OsuBeatmapId = suggestion.OsuBeatmapId
                                EtternaPackId = suggestion.EtternaPackId
                                Artist = suggestion.Artist
                                Title = suggestion.Title
                                Creator = suggestion.Creator
                                Difficulty = suggestion.Difficulty
                                TableFor = Table.current().Value.Name.ToLower()
                                SuggestedLevel = level.Rank
                            } : Tables.Suggestions.Add.Request),
                            function
                            | Some true ->
                                Notifications.action_feedback (Icons.FOLDER_PLUS, "Suggestion sent!", "")
                            | _ -> Notifications.error ("Error sending suggestion", "")
                        )
                        Menu.Back()
                ).Show()
            ))

        fc

    member this.Actions = actions

    override this.Init(parent: Widget) =
        this
        |+ Text(
            suggestion.Title, 
            Position = Position.Row(0.0f, 40.0f).Margin(10.0f, 0.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            sprintf "%s  •  %s" suggestion.Artist suggestion.Creator,
            Position = Position.Row(40.0f, 30.0f).Margin(10.0f, 0.0f),
            Align = Alignment.LEFT
        )
        |+ Text(
            suggestion.Difficulty,
            Position = Position.Row(70.0f, 30.0f).Margin(10.0f, 0.0f),
            Align = Alignment.LEFT,
            Color = K Colors.text_greyout
        )
        |+ Text(
            sprintf
                "Suggested: %s"
                (suggestion.LevelsSuggestedBy
                 |> Map.toSeq
                 |> Seq.map (fun (level, users) -> sprintf "%i by %s" level (String.concat ", " users))
                 |> String.concat ";"),
            Position = Position.Row(100.0f, 30.0f).Margin(10.0f, 0.0f),
            Align = Alignment.LEFT
        )
        |* actions

        base.Init parent

    override this.Draw() =
        base.Draw()
        Draw.rect (this.Bounds.SliceBottom(30.0f)) Colors.black.O2

type SuggestionsList() =
    inherit
        WebRequestContainer<Tables.Suggestions.List.Response>(
            fun this ->
                if Network.status = Network.Status.LoggedIn then
                    match Table.current () with
                    | Some table ->
                        Tables.Suggestions.List.get (
                            table.Name.ToLower(),
                            fun response ->
                                sync
                                <| fun () ->
                                    match response with
                                    | Some result -> this.SetData result
                                    | None -> this.ServerError()
                        )
                    | None -> failwith "impossible"
                else
                    this.Offline()
            , fun data ->
                let fc = FlowContainer.Vertical<Suggestion>(130.0f, Spacing = 30.0f)

                for s in data.Suggestions do
                    fc.Add(Suggestion(s))

                sync (fun () -> fc.Focus())

                ScrollContainer.Flow(fc, Position = Position.Margin(100.0f, 200.0f), Margin = 5.0f)
        )

type SuggestionsPage() as this =
    inherit Page()

    let sl = SuggestionsList()

    do 
        this.Content(
            StaticContainer(NodeType.Leaf)
            |+ sl
            |+ WIP()
        )

    override this.Title = %"table.suggestions.name"
    override this.OnClose() = ()
    override this.OnReturnTo() = sl.Reload()
