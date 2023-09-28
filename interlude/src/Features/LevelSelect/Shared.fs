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

    let refresh_all() = sync(refresh_all_event.Trigger)
    let refresh_details() = sync(refresh_details_event.Trigger)

    let on_refresh_all = refresh_all_event.Publish
    let on_refresh_details = refresh_details_event.Publish

    do Interlude.Features.Import.Import.charts_updated.Add refresh_all

    let play() =
        
        Chart.wait_for_load <| fun () ->

        if Network.lobby.IsSome then
            Lobby.select_chart(Chart.CACHE_DATA.Value, rate.Value, selectedMods.Value)
            Screen.change Screen.Type.Lobby Transitions.Flags.Default
        else
            Chart.SAVE_DATA.Value.LastPlayed <- DateTime.UtcNow
            Screen.changeNew
                ( fun () -> 
                    if autoplay then ReplayScreen.replay_screen(ReplayMode.Auto) :> Screen.T
                    else PlayScreen.play_screen(if enablePacemaker then PacemakerMode.Setting else PacemakerMode.None) )
                ( if autoplay then Screen.Type.Replay else Screen.Type.Play )
                Transitions.Flags.Default

    let challengeScore(_rate, _mods, replay) =
        // todo: not having save data should be impossible
        match Chart.SAVE_DATA with
        | Some data ->
            data.LastPlayed <- DateTime.UtcNow
            rate.Set _rate
            selectedMods.Set _mods
            Screen.changeNew
                ( fun () -> PlayScreen.play_screen(PacemakerMode.Score (rate.Value, replay)) )
                ( Screen.Type.Play )
                Transitions.Flags.Default
        | None -> Logging.Warn "There is no chart selected"