namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Data
open Prelude.Gameplay
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.Play
open Interlude.Features.Play.HUD

module ReplayScreen =

    let replay_screen (chart: Chart, mode: ReplayMode) =

        let replay_data, is_auto, rate, with_colors =
            match mode with
            | ReplayMode.Auto with_colors ->
                StoredReplayProvider.AutoPlay(with_colors.Keys, with_colors.Source.Notes) :> IReplayProvider,
                true,
                SelectedChart.rate.Value,
                with_colors
            | ReplayMode.Replay(score_info, with_colors) ->
                StoredReplayProvider(score_info.Replay) :> IReplayProvider, false, score_info.Rate, with_colors

        let FIRST_NOTE = with_colors.FirstNote
        let ruleset = Rulesets.current

        let state =
            {
                ShowInputOverlay = Setting.simple false
                ShowHitOverlay = Setting.simple false
                PlayfieldDim = Setting.percentf 0.5f
                IsAuto = is_auto
            }

        let playback_speed =
            Setting.bounded rate 0.5f 2.0f |> Setting.trigger (fun r -> Song.change_rate r)

        let mutable replay_data = replay_data

        let mutable scoring =
            Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes rate

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplayProvider(replay_data.GetFullReplay())
            scoring <- Metrics.create ruleset with_colors.Keys replay_data with_colors.Source.Notes rate
            screen.State.ChangeScoring scoring

        { new IPlayScreen(chart, with_colors, PacemakerState.None, scoring) with
            override this.AddWidgets() =
                let hud_config = Content.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, hud_config) position constructor

                if hud_config.ComboEnabled then add_widget hud_config.ComboPosition Combo
                if hud_config.SkipButtonEnabled then add_widget hud_config.SkipButtonPosition SkipButton
                if hud_config.ProgressMeterEnabled then add_widget hud_config.ProgressMeterPosition ProgressMeter

                if not is_auto then
                    if hud_config.AccuracyEnabled then add_widget hud_config.AccuracyPosition Accuracy
                    if hud_config.TimingDisplayEnabled then 
                        add_widget hud_config.TimingDisplayPosition 
                            (fun x -> TimingDisplay(x).Conditional(state.ShowHitOverlay.Get >> not))
                    if hud_config.JudgementCounterEnabled then add_widget hud_config.JudgementCounterPosition JudgementCounter
                    if hud_config.JudgementMeterEnabled then add_widget hud_config.JudgementMeterPosition JudgementMeter
                    if hud_config.EarlyLateMeterEnabled then add_widget hud_config.EarlyLateMeterPosition EarlyLateMeter
                    if hud_config.RateModMeterEnabled then add_widget hud_config.RateModMeterPosition RateModMeter
                    if hud_config.BPMMeterEnabled then add_widget hud_config.BPMMeterPosition BPMMeter
                    if hud_config.InputMeterEnabled then 
                        add_widget hud_config.InputMeterPosition
                            (fun (hud_config, play_state) -> InputMeter(hud_config, play_state, fun () -> not state.ShowInputOverlay.Value))
                if hud_config.CustomImageEnabled then add_widget hud_config.CustomImagePosition CustomImage

                this
                |+ { new StaticWidget(NodeType.None) with
                       override _.Draw() =
                           if state.ShowInputOverlay.Value || state.ShowHitOverlay.Value then
                               Draw.rect
                                   this.Playfield.Bounds
                                   (Colors.black.O4a(255.0f * state.PlayfieldDim.Value |> int))
                   }
                |+ InputOverlay(
                    with_colors.Keys,
                    replay_data.GetFullReplay(),
                    this.State,
                    this.Playfield,
                    state.ShowInputOverlay
                )
                |+ HitOverlay(
                    rate,
                    with_colors.Source,
                    replay_data.GetFullReplay(),
                    this.State,
                    this.Playfield,
                    state.ShowHitOverlay
                )
                |* ControlOverlay(
                    with_colors.Source,
                    state,
                    fun t ->
                        let now = Song.time () in
                        Song.seek t

                        if t < now then
                            seek_backwards this
                )

            override this.OnEnter p =
                DiscordRPC.playing ("Watching a replay", SelectedChart.CACHE_DATA.Value.Title)
                base.OnEnter p

            override this.OnExit p =
                base.OnExit p
                Song.change_rate SelectedChart.rate.Value
                Toolbar.show_cursor ()
                Song.resume()

            override this.Update(elapsed_ms, moved) =
                base.Update(elapsed_ms, moved)
                let now = Song.time_with_offset ()
                let chart_time = now - FIRST_NOTE

                if not replay_data.Finished then
                    scoring.Update chart_time

                if replay_data.Finished then
                    match mode with
                    | ReplayMode.Auto _ -> Screen.back Transitions.LeaveGameplay |> ignore
                    | ReplayMode.Replay(score_info, _) ->
                        Screen.change_new
                            (fun () -> new ScoreScreen(score_info, ImprovementFlags.None, false) :> Screen)
                            Screen.Type.Score
                            Transitions.EnterGameplay
                        |> ignore

                if (%%"skip").Tapped() then
                    if Song.playing () then (if Song.time () > 0.0f<ms> then Song.pause ()) else Song.resume ()
                else
                    SelectedChart.change_rate_hotkeys (fun change_by -> playback_speed.Value <- playback_speed.Value + change_by)
        }