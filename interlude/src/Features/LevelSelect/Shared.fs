namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Data.Scores
open Prelude.Data.Charts.Sorting
open Prelude.Data.Charts.Endless
open Interlude.Content
open Interlude.UI
open Interlude.Options
open Interlude.Features.Play
open Interlude.Features.Online
open Interlude.Features.Gameplay

module LevelSelect =

    let private refresh_all_event = Event<unit>()
    let private refresh_details_event = Event<unit>()

    let refresh_all () = sync (refresh_all_event.Trigger)
    let refresh_details () = sync (refresh_details_event.Trigger)

    let on_refresh_all = refresh_all_event.Publish
    let on_refresh_details = refresh_details_event.Publish
    
    let mutable filter: Filter = []

    do 
        Interlude.Features.Import.Import.charts_updated.Add refresh_all
        Interlude.Features.Collections.CollectionActions.collection_modified.Add (fun () ->
            if options.LibraryMode.Value = LibraryMode.Collections then 
                refresh_all ()
            else
                refresh_details ()
        )

    let try_play (info: Chart.LoadedChartInfo) : bool =
        if
            Screen.change_new
                (fun () ->
                    PlayScreen.play_screen (
                        info,
                        if options.EnablePacemaker.Value then PacemakerMode.Setting else PacemakerMode.None
                    )
                )
                Screen.Type.Play
                Transitions.Flags.Default
        then 
            // todo: move to play/multiplay screens?
            info.SaveData.LastPlayed.Value <- Timestamp.now()
            true
        else false

    let choose_this_chart () =

        Chart.when_loaded <| fun info ->

        if Network.lobby.IsSome then
            if Screen.change Screen.Type.Lobby Transitions.Flags.Default then
                Lobby.select_chart (info.CacheInfo, rate.Value, selected_mods.Value)
        elif
            if autoplay then
                Screen.change_new
                    (fun () -> ReplayScreen.replay_screen (info.Chart, ReplayMode.Auto info.WithColors) :> Screen.T)
                    Screen.Type.Replay
                    Transitions.Flags.Default
            else try_play info
        then
            if endless_mode.Value then 
                Endless.begin_endless_mode <| 
                    EndlessModeState.create {
                        BaseChart = info.CacheInfo
                        Filter = filter
                        Rate = rate.Value
                        Mods = selected_mods.Value
                        ScoreDatabase = Content.Scores
                    }

    let challenge_score (score_info: ScoreInfo) =
        Chart.if_loaded <| fun info ->

        if
            Screen.change_new
                (fun () -> PlayScreen.play_screen (info, PacemakerMode.Score(score_info.Rate, score_info.Replay)))
                Screen.Type.Play
                Transitions.Flags.Default
        then
            // todo: move to play/multiplay screens?
            info.SaveData.LastPlayed.Value <- Timestamp.now()
            rate.Set score_info.Rate
            selected_mods.Set score_info.Mods