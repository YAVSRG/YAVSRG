namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Common
open Prelude
open Prelude.Charts
open Prelude.Charts.Processing
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Gameplay
open Prelude.Data.User
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

        let replay_data, is_auto, rate, with_colors, is_failed =
            match mode with
            | ReplayMode.Auto with_colors ->
                StoredReplayProvider.AutoPlay(with_colors.Keys, with_colors.Source.Notes) :> IReplayProvider,
                true,
                SelectedChart.rate.Value,
                with_colors,
                false
            | ReplayMode.Replay(score_info, with_colors) ->
                StoredReplayProvider(score_info.Replay) :> IReplayProvider,
                false,
                score_info.Rate,
                with_colors,
                score_info.IsFailed

        let FIRST_NOTE = with_colors.FirstNote
        let ruleset = Rulesets.current

        let replay_ended_fade = Animation.Fade 0.0f

        let mutable replay_data = replay_data

        let mutable scoring =
            ScoreProcessor.create ruleset with_colors.Keys replay_data with_colors.Source.Notes rate

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplayProvider(replay_data.GetFullReplay())
            scoring <- ScoreProcessor.create ruleset with_colors.Keys replay_data with_colors.Source.Notes rate
            screen.State.ChangeScoring scoring

        { new IPlayScreen(chart, with_colors, PacemakerState.None, scoring) with
            override this.AddWidgets() =
                let hud_config = Content.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, hud_config) position constructor

                if hud_config.ComboEnabled then add_widget hud_config.ComboPosition Combo
                if hud_config.ProgressMeterEnabled then add_widget hud_config.ProgressMeterPosition ProgressMeter

                if not is_auto then
                    if hud_config.AccuracyEnabled then add_widget hud_config.AccuracyPosition Accuracy
                    if hud_config.TimingDisplayEnabled then 
                        add_widget hud_config.TimingDisplayPosition 
                            (fun x -> TimingDisplay(x).Conditional(show_hit_overlay.Get >> not))
                    if hud_config.JudgementCounterEnabled then add_widget hud_config.JudgementCounterPosition JudgementCounter
                    if hud_config.JudgementMeterEnabled then add_widget hud_config.JudgementMeterPosition JudgementMeter
                    if hud_config.EarlyLateMeterEnabled then add_widget hud_config.EarlyLateMeterPosition EarlyLateMeter
                if hud_config.RateModMeterEnabled then add_widget hud_config.RateModMeterPosition RateModMeter
                if hud_config.BPMMeterEnabled then add_widget hud_config.BPMMeterPosition BPMMeter
                if hud_config.InputMeterEnabled then add_widget hud_config.InputMeterPosition InputMeter
                if hud_config.KeysPerSecondMeterEnabled then add_widget hud_config.KeysPerSecondMeterPosition KeysPerSecondMeter
                if hud_config.CustomImageEnabled then add_widget hud_config.CustomImagePosition CustomImage

                this
                |+ { new StaticWidget(NodeType.None) with
                       override _.Draw() =
                           if show_input_overlay.Value || show_hit_overlay.Value then
                               Draw.rect
                                   this.Playfield.Bounds
                                   (Colors.black.O4a(255.0f * playfield_dim.Value |> int))
                   }
                |+ InputOverlay(
                    with_colors.Keys,
                    replay_data.GetFullReplay(),
                    this.State,
                    this.Playfield
                )
                |+ HitOverlay(
                    rate,
                    with_colors.Source,
                    replay_data.GetFullReplay(),
                    this.State,
                    this.Playfield
                )
                |+ Text(
                    %"replay.end_of_data",
                    Color = (fun () -> Colors.red_accent.O4a replay_ended_fade.Alpha, Colors.shadow_2.O4a replay_ended_fade.Alpha),
                    Position = Position.ShrinkB(100.0f).SliceB(60.0f)
                )
                |* ReplayControls(
                    with_colors.Source,
                    is_auto,
                    rate,
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
                replay_ended_fade.Target <- if is_failed && replay_data.Finished then 1.0f else 0.0f
                replay_ended_fade.Update elapsed_ms
                let now = Song.time_with_offset ()
                let chart_time = now - FIRST_NOTE

                scoring.Update chart_time

                if replay_data.Finished && now > chart.LastNote then
                    match mode with
                    | ReplayMode.Auto _ -> Screen.back Transitions.LeaveGameplay |> ignore
                    | ReplayMode.Replay(score_info, _) ->
                        Screen.change_new
                            (fun () -> new ScoreScreen(score_info, (ImprovementFlags.None, None), false) :> Screen)
                            Screen.Type.Score
                            Transitions.EnterGameplayNoFadeAudio
                        |> ignore
        }