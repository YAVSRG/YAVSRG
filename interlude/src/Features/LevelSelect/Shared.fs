namespace Interlude.Features.LevelSelect

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Charts.Processing
open Prelude.Data
open Prelude.Data.Library.Sorting
open Interlude.UI
open Interlude.Options
open Interlude.Features.Pacemaker
open Interlude.Features.Play
open Interlude.Features.Online
open Interlude.Features.Import
open Interlude.Features.Collections
open Interlude.Features.Gameplay

module LevelSelect =

    let private refresh_all_event = Event<unit>()
    let private refresh_details_event = Event<unit>()

    let refresh_all () = defer (refresh_all_event.Trigger)
    let refresh_details () = defer (refresh_details_event.Trigger)

    let on_refresh_all = refresh_all_event.Publish
    let on_refresh_details = refresh_details_event.Publish

    let mutable filter: Filter = []

    do
        charts_updated.Add (fun () -> if Screen.current_type = Screen.Type.LevelSelect then refresh_all())

        CollectionActions.collection_modified.Add(fun () ->
            if options.LibraryMode.Value = LibraryMode.Collections then
                refresh_all ()
            else
                refresh_details ()
        )

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
            Transitions.Gameplay

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
                    Transitions.Gameplay
            then
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