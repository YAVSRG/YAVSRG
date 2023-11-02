namespace Interlude.Features.LevelSelect

open System
open Percyqaz.Common
open Percyqaz.Flux.UI
open Interlude.UI
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

        Chart.wait_for_load
        <| fun () ->

            if Network.lobby.IsSome then
                if Screen.change Screen.Type.Lobby Transitions.Flags.Default then
                    Lobby.select_chart (Chart.CACHE_DATA.Value, rate.Value, selected_mods.Value)
            else if
                Screen.change_new
                    (fun () ->
                        if autoplay then
                            ReplayScreen.replay_screen (ReplayMode.Auto) :> Screen.T
                        else
                            PlayScreen.play_screen (
                                if enable_pacemaker then
                                    PacemakerMode.Setting
                                else
                                    PacemakerMode.None
                            )
                    )
                    (if autoplay then Screen.Type.Replay else Screen.Type.Play)
                    Transitions.Flags.Default
            then
                Chart.SAVE_DATA.Value.LastPlayed <- DateTime.UtcNow

    let challengeScore (_rate, _mods, replay) =
        match Chart.SAVE_DATA with
        | Some data ->
            if
                Screen.change_new
                    (fun () -> PlayScreen.play_screen (PacemakerMode.Score(rate.Value, replay)))
                    (Screen.Type.Play)
                    Transitions.Flags.Default
            then
                data.LastPlayed <- DateTime.UtcNow
                rate.Set _rate
                selected_mods.Set _mods

        | None -> Logging.Warn "There is no chart selected"
