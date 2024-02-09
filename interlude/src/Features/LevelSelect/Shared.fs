namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Data.Scores
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

    do Interlude.Features.Import.Import.charts_updated.Add refresh_all

    let play () =

        Chart.when_loaded <| fun info ->

        if Network.lobby.IsSome then
            if Screen.change Screen.Type.Lobby Transitions.Flags.Default then
                Lobby.select_chart (info.CacheInfo, rate.Value, selected_mods.Value)
        else if
            Screen.change_new
                (fun () ->
                    if autoplay then
                        ReplayScreen.replay_screen (info.Chart, ReplayMode.Auto info.WithColors) :> Screen.T
                    else
                        PlayScreen.play_screen (info,
                            if options.EnablePacemaker.Value then
                                PacemakerMode.Setting
                            else
                                PacemakerMode.None
                        )
                )
                (if autoplay then Screen.Type.Replay else Screen.Type.Play)
                Transitions.Flags.Default
        then
            info.SaveData.LastPlayed <- DateTime.UtcNow

    let challenge_score (score_info: ScoreInfo) =
        Chart.if_loaded <| fun info ->

        if
            Screen.change_new
                (fun () -> PlayScreen.play_screen (info, PacemakerMode.Score(score_info.Rate, score_info.Replay)))
                Screen.Type.Play
                Transitions.Flags.Default
        then
            info.SaveData.LastPlayed <- DateTime.UtcNow
            rate.Set score_info.Rate
            selected_mods.Set score_info.Mods
