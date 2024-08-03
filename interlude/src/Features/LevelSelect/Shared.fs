namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude
open Prelude.Charts.Processing
open Prelude.Data
open Prelude.Data.Library.Sorting
open Prelude.Data.Library.Collections
open Prelude.Data.Library.Endless
open Prelude.Data.Library.Caching
open Interlude.UI
open Interlude.Content
open Interlude.Options
open Interlude.Features.Pacemaker
open Interlude.Features.Play
open Interlude.Features.Online
open Interlude.Features.Collections
open Interlude.Features.Gameplay

module LevelSelect =

    let private refresh_all_event = Event<unit>()
    let private refresh_details_event = Event<unit>()

    let refresh_all () = defer (refresh_all_event.Trigger)
    let refresh_details () = defer (refresh_details_event.Trigger)

    let on_refresh_all = refresh_all_event.Publish
    let on_refresh_details = refresh_details_event.Publish

    do
        Content.OnChartAdded.Add (fun () -> if Screen.current_type = Screen.Type.LevelSelect then refresh_all())

        CollectionActions.collection_modified.Add(fun () ->
            if options.LibraryMode.Value = LibraryMode.Collections then
                refresh_all ()
            else
                refresh_details ()
        )

    let mutable filter: Filter = []
    let endless_priority = Setting.simple SuggestionPriority.Variety

    module History =

        let mutable private history: CachedChart list = []

        let append_current () = 
            match SelectedChart.CACHE_DATA with
            | Some cc -> history <- cc :: history
            | None -> ()

        let previous () =
            if not (Transitions.in_progress()) then
                match history with
                | cc :: xs -> 
                    history <- xs
                    SelectedChart.change(cc, LibraryContext.None, true)
                | _ -> ()

        let has_previous() = (List.isEmpty >> not) history

    let private state: EndlessModeState = EndlessModeState.create()
    let mutable private suggestion_ctx : SuggestionContext option = None

    let private enter_gameplay (ctx: LibraryContext) =
        match ctx with
        | LibraryContext.Playlist(index, playlist_id, data) ->
            match Content.Library.Collections.GetPlaylist playlist_id with
            | Some playlist -> EndlessModeState.queue_playlist (index + 1) playlist Content.Library state
            | None -> ()
        | _ -> ()

    let try_play (info: LoadedChartInfo) : bool =
        Screen.change_new
            (fun () ->
                PlayScreen.play_screen (
                    info,
                    if options.EnablePacemaker.Value then
                        PacemakerCreationContext.FromUserSetting
                    else
                        PacemakerCreationContext.None
                )
            )
            Screen.Type.Play
            Transitions.EnterGameplay
        |> function true -> enter_gameplay info.LibraryContext; true | false -> false

    // todo: remove the holes from this system by making a proper way to wait for song to load
    let rec private retry_until_song_loaded (info: LoadedChartInfo) (action: LoadedChartInfo -> bool) =
        if not (action info) then
            defer (fun () -> retry_until_song_loaded info action)

    let continue_endless_mode () : bool =
        let ctx =
            match suggestion_ctx with
            | None ->
                {
                    BaseDifficulty = SelectedChart.RATING.Value.Physical
                    BaseChart = SelectedChart.CACHE_DATA.Value, SelectedChart.rate.Value
                    Filter = filter |> Filter.except_keywords
                    Mods = SelectedChart.selected_mods.Value
                    RulesetId = Rulesets.current_hash
                    Ruleset = Rulesets.current
                    Library = Content.Library
                    ScoreDatabase = Content.Scores
                    Priority = endless_priority.Value
                }
            | Some ctx -> ctx

        match EndlessModeState.next ctx state with
        | Some next ->
            History.append_current()
            SelectedChart._rate.Set next.Rate
            SelectedChart._selected_mods.Set next.Mods
            SelectedChart.change (next.Chart, LibraryContext.None, false)
            SelectedChart.when_loaded <| (fun info -> retry_until_song_loaded info try_play)
            suggestion_ctx <- Some next.NextContext
            true
        | None ->
            false

    let start_playlist (playlist: Playlist) =
        EndlessModeState.queue_playlist 0 playlist Content.Library state
        continue_endless_mode() |> ignore

    let start_playlist_shuffled (playlist: Playlist) =
        EndlessModeState.queue_shuffled_playlist playlist Content.Library state
        continue_endless_mode() |> ignore
    
    let exit_gameplay () =
        suggestion_ctx <- None
        EndlessModeState.clear_queue state

    let random_chart () =
        if not (Transitions.in_progress()) then
            if SelectedChart.RATING.IsSome then
                let ctx =
                    {
                        BaseDifficulty = SelectedChart.RATING.Value.Physical
                        BaseChart = SelectedChart.CACHE_DATA.Value, SelectedChart.rate.Value
                        Filter = filter |> Filter.except_keywords
                        Mods = SelectedChart.selected_mods.Value
                        RulesetId = Rulesets.current_hash
                        Ruleset = Rulesets.current
                        Library = Content.Library
                        ScoreDatabase = Content.Scores
                        Priority = endless_priority.Value
                    }

                match Suggestion.get_suggestion ctx with
                | Some (cc, rate) ->
                    History.append_current()
                    SelectedChart._rate.Value <- rate
                    SelectedChart.change(cc, LibraryContext.None, true)
                | None -> 
                    Notifications.action_feedback (Icons.ALERT_CIRCLE, %"notification.suggestion_failed", "")
                    if Screen.current_type = Screen.Type.MainMenu then Percyqaz.Flux.Audio.Song.play_leadin()
            else
                let ctx =
                    {
                        Rate = SelectedChart.rate.Value
                        RulesetId = Rulesets.current_hash
                        Ruleset = Rulesets.current
                        Library = Content.Library
                        ScoreDatabase = Content.Scores
                    }

                match Suggestion.get_random filter ctx with
                | Some cc ->
                    History.append_current()
                    SelectedChart.change(cc, LibraryContext.None, true)
                | None -> ()

    let choose_this_chart () =
        SelectedChart.when_loaded
        <| fun info ->

            match Network.lobby with
            | Some lobby ->
                if Screen.change Screen.Type.Lobby Transitions.Default then
                    lobby.SelectChart (info.CacheInfo, SelectedChart.rate.Value, SelectedChart.selected_mods.Value)
            | None ->
                if SelectedChart.autoplay then
                    Screen.change_new
                        (fun () -> ReplayScreen.replay_screen (info.Chart, ReplayMode.Auto info.WithColors) :> Screen.T)
                        Screen.Type.Replay
                        Transitions.Default
                else
                    try_play info
                |> ignore

    let challenge_score (score_info: ScoreInfo) =
        SelectedChart.if_loaded
        <| fun info ->

            if
                Screen.change_new
                    (fun () -> PlayScreen.play_screen (info, PacemakerCreationContext.FromScore score_info))
                    Screen.Type.Play
                    Transitions.EnterGameplay
            then
                enter_gameplay info.LibraryContext
                SelectedChart.rate.Set score_info.Rate
                SelectedChart.selected_mods.Set score_info.Mods

    let watch_replay (score_info: ScoreInfo, with_colors: ColoredChart) =
        if
            Screen.change_new
                (fun () ->
                    ReplayScreen.replay_screen (score_info.Chart, ReplayMode.Replay(score_info, with_colors))
                    :> Screen.T
                )
                Screen.Type.Replay
                Transitions.Default
        then
            SelectedChart.rate.Value <- score_info.Rate