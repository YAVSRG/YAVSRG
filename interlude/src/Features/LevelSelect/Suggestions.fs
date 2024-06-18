namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Prelude.Data.Library.Caching
open Prelude.Data.Library.Sorting
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Content
open Interlude.Options

module Suggestions =

    let mutable private history: CachedChart list = []

    let add_current_chart_to_history () = 
        match SelectedChart.CACHE_DATA with
        | Some cc -> history <- cc :: history
        | None -> ()

    let previous_chart () =
        if not (Transitions.in_progress()) then
            match history with
            | cc :: xs -> 
                history <- xs
                SelectedChart.change(cc, LibraryContext.None, true)
            | _ -> ()

    let has_previous() = (List.isEmpty >> not) history

    // todo: remove the holes from this system by making a proper way to wait for song to load
    let rec retry_until_song_loaded (info: LoadedChartInfo) (action: LoadedChartInfo -> bool) =
        if not (action info) then
            defer (fun () -> retry_until_song_loaded info action)

    let endless_priority = Setting.simple SuggestionPriority.Variety
    let mutable private state: EndlessModeState option = None

    let exit_endless_mode () = state <- None

    let in_endless_mode () : bool = state.IsSome

    let continue_endless_mode () : bool =
        match state with
        | Some endless_mode ->
            match EndlessModeState.next endless_mode with
            | Some next ->
                add_current_chart_to_history()
                SelectedChart._rate.Set next.Rate
                SelectedChart._selected_mods.Set next.Mods
                SelectedChart.change (next.Chart, LibraryContext.None, false)
                SelectedChart.when_loaded <| (fun info -> retry_until_song_loaded info LevelSelect.try_play)
                state <- Some next.NewState
                true
            | None ->
                Notifications.action_feedback (Icons.ALERT_CIRCLE, %"notification.suggestion_failed", "")
                exit_endless_mode ()
                false
        | None -> false

    let begin_endless_mode (initial_state: EndlessModeState) (skip_current_song: bool) = 
        state <- Some initial_state
        if skip_current_song then 
            continue_endless_mode() |> ignore 
        else 
            SelectedChart.if_loaded (fun info -> if not (LevelSelect.try_play info) then exit_endless_mode())

    let random_chart () =
        if not (Transitions.in_progress()) then
            if SelectedChart.RATING.IsSome then
                let ctx =
                    {
                        BaseDifficulty = SelectedChart.RATING.Value.Physical
                        BaseChart = SelectedChart.CACHE_DATA.Value, SelectedChart.rate.Value
                        Filter = LevelSelect.filter |> Filter.except_keywords
                        Mods = SelectedChart.selected_mods.Value
                        RulesetId = Rulesets.current_hash
                        Ruleset = Rulesets.current
                        Library = Content.Library
                        ScoreDatabase = Content.Scores
                        Priority = endless_priority.Value
                    }

                match Suggestion.get_suggestion ctx with
                | Some (cc, rate) ->
                    add_current_chart_to_history()
                    SelectedChart._rate.Value <- rate
                    SelectedChart.change(cc, LibraryContext.None, true)
                | None -> Notifications.action_feedback (Icons.ALERT_CIRCLE, %"notification.suggestion_failed", "")
            else
                let ctx =
                    {
                        Rate = SelectedChart.rate.Value
                        RulesetId = Rulesets.current_hash
                        Ruleset = Rulesets.current
                        Library = Content.Library
                        ScoreDatabase = Content.Scores
                    }

                match Suggestion.get_random LevelSelect.filter ctx with
                | Some cc ->
                    add_current_chart_to_history()
                    SelectedChart.change(cc, LibraryContext.None, true)
                | None -> ()