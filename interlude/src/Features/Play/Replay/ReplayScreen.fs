﻿namespace Interlude.Features.Play.Replay

open Percyqaz.Flux.Audio
open Percyqaz.Flux.UI
open Percyqaz.Flux.Graphics
open Percyqaz.Common
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Data.User
open Interlude.Content
open Interlude.UI
open Interlude.Features.Gameplay
open Interlude.Features.Pacemaker
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.Play
open Interlude.Features.Play.HUD

type ReplayScreen =

    static member Create(info: LoadedChartInfo, mode: ReplayMode) : Screen =

        let replay_data, is_auto, rate, is_failed =
            match mode with
            | ReplayMode.Auto ->
                StoredReplay.AutoPlay(info.WithColors.Keys, info.WithMods.Notes) :> IReplay,
                true,
                SelectedChart.rate.Value,
                false
            | ReplayMode.Replay score_info ->
                StoredReplay(score_info.Replay) :> IReplay,
                false,
                score_info.Rate,
                score_info.IsFailed

        let LAST_NOTE = info.WithColors.LastNote
        let ruleset = Rulesets.current

        let replay_ended_fade = Animation.Fade 0.0f

        let mutable last_time = -Time.infinity
        let mutable replay_data = replay_data

        let mutable scoring =
            ScoreProcessor.create ruleset info.WithMods.Keys replay_data info.WithMods.Notes rate

        let seek_backwards (screen: IPlayScreen) =
            replay_data <- StoredReplay(replay_data.GetFullReplay())
            scoring <- ScoreProcessor.create ruleset info.WithMods.Keys replay_data info.WithMods.Notes rate
            screen.State.ChangeScoring scoring

        { new IPlayScreen(info, PacemakerState.None, scoring) with
            override this.AddWidgets() =
                let hud_config = Content.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, hud_config) position constructor

                if hud_config.ComboEnabled then add_widget hud_config.ComboPosition Combo
                if hud_config.ProgressMeterEnabled then add_widget hud_config.ProgressMeterPosition ProgressPie

                if not is_auto then
                    if hud_config.AccuracyEnabled then add_widget hud_config.AccuracyPosition Accuracy
                    if hud_config.TimingDisplayEnabled then
                        add_widget hud_config.TimingDisplayPosition
                            (fun x -> ErrorBar(x).Conditional(show_hit_overlay.Get >> not))
                    if hud_config.JudgementCounterEnabled then add_widget hud_config.JudgementCounterPosition JudgementCounter
                    if hud_config.JudgementMeterEnabled then add_widget hud_config.JudgementMeterPosition Judgement
                    if hud_config.EarlyLateMeterEnabled then add_widget hud_config.EarlyLateMeterPosition EarlyLate
                if hud_config.RateModMeterEnabled then add_widget hud_config.RateModMeterPosition RateMods
                if hud_config.BPMMeterEnabled then add_widget hud_config.BPMMeterPosition BPM
                if hud_config.InputMeterEnabled then add_widget hud_config.InputMeterPosition InputMeter
                if hud_config.KeysPerSecondMeterEnabled then add_widget hud_config.KeysPerSecondMeterPosition KeysPerSecond
                if hud_config.CustomImageEnabled then add_widget hud_config.CustomImagePosition CustomImage
                if hud_config.HudGraphEnabled then add_widget hud_config.HudGraphPosition HudGraph

                this
                    .Add(
                        { new StaticWidget(NodeType.None) with
                            override _.Draw() =
                                if show_input_overlay.Value || show_hit_overlay.Value then
                                    Render.rect
                                        this.Playfield.Bounds
                                        (Colors.black.O4a(255.0f * playfield_dim.Value |> int))
                        },
                        InputOverlay(
                            info.WithMods.Keys,
                            replay_data.GetFullReplay(),
                            this.State,
                            this.Playfield
                        ),
                        HitOverlay(
                            rate,
                            info.WithMods,
                            replay_data.GetFullReplay(),
                            this.State,
                            this.Playfield
                        ),
                        DifficultyOverlay(
                            info.WithMods,
                            this.Playfield,
                            info.Difficulty,
                            this.State
                        )
                            .Conditional(show_difficulty_overlay.Get),
                        Text(%"replay.end_of_data")
                            .Color((fun () -> Colors.red_accent.O4a replay_ended_fade.Alpha, Colors.shadow_2.O4a replay_ended_fade.Alpha))
                            .Position(Position.ShrinkB(100.0f).SliceB(60.0f)),
                        ReplayControls(
                            info.WithMods,
                            is_auto,
                            rate,
                            fun t -> Song.seek t
                        )
                    )

            override this.OnEnter p =
                Song.change_rate rate
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
                let now = this.State.CurrentTime()
                let chart_time = this.State.CurrentChartTime()

                if last_time > now then
                    seek_backwards(this)
                last_time <- now

                this.State.Scoring.Update chart_time

                if now > LAST_NOTE && replay_data.Finished then
                    match mode with
                    | ReplayMode.Auto -> Screen.back Transitions.LeaveGameplay |> ignore
                    | ReplayMode.Replay score_info ->
                        Screen.change_new
                            (fun () -> new ScoreScreen(score_info, (ImprovementFlags.None, None), false) :> Screen)
                            ScreenType.Score
                            Transitions.EnterGameplayNoFadeAudio
                        |> ignore
        }