namespace Interlude.Features.Gameplay

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Interlude.UI

module Endless =

    let priority = Setting.simple SuggestionPriority.Variety

    // todo: remove the holes from this system by making a proper way to wait for song to load
    let rec retry_until_song_loaded (info: LoadedChartInfo) (action: LoadedChartInfo -> bool) =
        if not (action info) then
            defer (fun () -> retry_until_song_loaded info action)

    let mutable private state: EndlessModeState option = None

    let begin_endless_mode (initial_state: EndlessModeState) = state <- Some initial_state

    let exit_endless_mode () = state <- None

    let continue_endless_mode (when_loaded: LoadedChartInfo -> bool) : bool =
        match state with
        | Some endless_mode ->
            match EndlessModeState.next endless_mode with
            | Some next ->
                Things.add_current_chart_to_history()
                SelectedChart._rate.Set next.Rate
                SelectedChart._selected_mods.Set next.Mods
                SelectedChart.change (next.Chart, LibraryContext.None, false)
                SelectedChart.when_loaded <| (fun info -> retry_until_song_loaded info when_loaded)
                state <- Some next.NewState
                true
            | None ->
                Notifications.action_feedback (Icons.ALERT_CIRCLE, %"notification.suggestion_failed", "")
                exit_endless_mode ()
                false
        | None -> false