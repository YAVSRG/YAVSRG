namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Data.User.Stats
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.Play.HUD

module PlayScreen =

    let SHOW_START_OVERLAY = true

    let rec play_screen (info: LoadedChartInfo, pacemaker_ctx: PacemakerCreationContext) =

        let ruleset = Rulesets.current
        let first_note = info.WithMods.FirstNote
        let liveplay = LiveReplayProvider first_note

        let scoring =
            ScoreProcessor.create ruleset info.WithMods.Keys liveplay info.WithMods.Notes SelectedChart.rate.Value

        let binds = options.GameplayBinds.[info.WithMods.Keys - 3]
        let mutable key_state = 0us
        let mutable liveplay_position = -Time.infinity

        let mutable play_time = 0.0

        scoring.OnEvent.Add(fun h ->
            match h.Action with
            | Hit d
            | Hold d when not d.Missed -> CURRENT_SESSION.NotesHit <- CURRENT_SESSION.NotesHit + 1
            | _ -> ()
        )

        let pacemaker_state = PacemakerState.create info pacemaker_ctx

        let mutable offset_manually_changed = false
        let offset_setting = LocalOffset.offset_setting info.SaveData

        let retry () =
            if
                Screen.change_new
                    (fun () -> play_screen (info, pacemaker_ctx) :> Screen)
                    ScreenType.Play
                    Transitions.EnterGameplayFadeAudio
            then
                CURRENT_SESSION.PlaysRetried <- CURRENT_SESSION.PlaysRetried + 1

        let fade_in = Animation.Fade (if SHOW_START_OVERLAY then 0.0f else 1.0f)
        let start_overlay = StartOverlay(
            info,
            pacemaker_state,
            fun () ->
                fade_in.Target <- 1.0f
                Background.dim (float32 options.BackgroundDim.Value)
                Background.set_parallax_amount 40.0f
        )

        let skip_song () =
            if Gameplay.continue_endless_mode() then CURRENT_SESSION.PlaysQuit <- CURRENT_SESSION.PlaysQuit + 1

        let give_up () =
            let is_giving_up_play = not (liveplay :> IReplayProvider).Finished && (Song.time() - first_note) / SelectedChart.rate.Value > 15000f<ms / rate>

            if is_giving_up_play then
                liveplay.Finish()
                scoring.Update Time.infinity
                match options.QuitOutBehaviour.Value with
                | QuitOutBehaviour.SaveAndShow
                | QuitOutBehaviour.Show ->
                    Screen.change_new
                        (fun () ->
                            let score_info =
                                Gameplay.score_info_from_gameplay
                                    info
                                    scoring
                                    ((liveplay :> IReplayProvider).GetFullReplay())
                                    true
                            ScoreScreen(score_info, Gameplay.set_score true score_info info.SaveData, true)
                        )
                        ScreenType.Score
                        Transitions.LeaveGameplay

                | QuitOutBehaviour.Ignore ->
                    let score_info =
                        Gameplay.score_info_from_gameplay
                            info
                            scoring
                            ((liveplay :> IReplayProvider).GetFullReplay())
                            true
                    Gameplay.set_score true score_info info.SaveData |> ignore
                    Screen.back Transitions.LeaveGameplay
            else
                Screen.back Transitions.LeaveGameplay
            |> function false -> () | true -> CURRENT_SESSION.PlaysQuit <- CURRENT_SESSION.PlaysQuit + 1

        let fail_midway (this: IPlayScreen) =
            liveplay.Finish()

            let view_score() =
                if
                    Screen.change_new
                        (fun () ->
                            scoring.Update Time.infinity
                            let score_info =
                                Gameplay.score_info_from_gameplay
                                    info
                                    scoring
                                    ((liveplay :> IReplayProvider).GetFullReplay())
                                    true

                            (score_info, Gameplay.set_score false score_info info.SaveData, true)
                            |> ScoreScreen
                        )
                        ScreenType.Score
                        Transitions.EnterGameplayNoFadeAudio
                then
                    CURRENT_SESSION.PlaysQuit <- CURRENT_SESSION.PlaysQuit + 1

            fade_in.Target <- 0.5f
            this |* FailOverlay(pacemaker_state, retry, view_score, skip_song)

        let finish_play (this: IPlayScreen) =
            liveplay.Finish()
            let pacemaker_met = PacemakerState.pacemaker_met scoring pacemaker_state

            let view_score() =
                if
                    Screen.change_new
                        (fun () ->
                            let score_info =
                                Gameplay.score_info_from_gameplay
                                    info
                                    scoring
                                    ((liveplay :> IReplayProvider).GetFullReplay())
                                    false

                            (score_info, Gameplay.set_score false score_info info.SaveData, true)
                            |> ScoreScreen
                        )
                        ScreenType.Score
                        Transitions.EnterGameplayNoFadeAudio
                then
                    CURRENT_SESSION.PlaysCompleted <- CURRENT_SESSION.PlaysCompleted + 1

            if pacemaker_met then
                view_score()
            else
                fade_in.Target <- 0.5f
                this |* FailOverlay(pacemaker_state, retry, view_score, skip_song)

        let change_offset (state: PlayState) =
            Song.pause()
            liveplay.Finish()
            offset_manually_changed <- true
            LocalOffsetPage(LocalOffset.get_automatic state info.SaveData, offset_setting, retry).Show()

        { new IPlayScreen(info.Chart, info.WithColors, pacemaker_state, scoring) with
            override this.AddWidgets() =
                let hud_config = Content.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, hud_config) position constructor

                if hud_config.ComboEnabled then add_widget hud_config.ComboPosition Combo
                add_widget hud_config.SkipButtonPosition SkipButton
                if hud_config.ProgressMeterEnabled then add_widget hud_config.ProgressMeterPosition ProgressPie
                if hud_config.AccuracyEnabled then add_widget hud_config.AccuracyPosition Accuracy
                if hud_config.TimingDisplayEnabled then add_widget hud_config.TimingDisplayPosition ErrorBar
                if this.State.Pacemaker <> PacemakerState.None then add_widget hud_config.PacemakerPosition Pacemaker
                if hud_config.JudgementCounterEnabled then add_widget hud_config.JudgementCounterPosition JudgementCounter
                if hud_config.JudgementMeterEnabled then add_widget hud_config.JudgementMeterPosition Judgement
                if hud_config.EarlyLateMeterEnabled then add_widget hud_config.EarlyLateMeterPosition EarlyLate
                if hud_config.RateModMeterEnabled then add_widget hud_config.RateModMeterPosition RateMods
                if hud_config.BPMMeterEnabled then add_widget hud_config.BPMMeterPosition BPM
                if hud_config.InputMeterEnabled then add_widget hud_config.InputMeterPosition InputMeter
                if hud_config.KeysPerSecondMeterEnabled then add_widget hud_config.KeysPerSecondMeterPosition KeysPerSecond
                if hud_config.CustomImageEnabled then add_widget hud_config.CustomImagePosition CustomImage

                this
                |+ HotkeyHoldAction(
                    "retry",
                    (if options.HoldToGiveUp.Value then ignore else retry),
                    (if options.HoldToGiveUp.Value then retry else ignore)
                )
                |+ HotkeyHoldAction(
                    "next_song",
                    (if options.HoldToGiveUp.Value then ignore else skip_song),
                    (if options.HoldToGiveUp.Value then skip_song else ignore)
                )
                |+ HotkeyHoldAction(
                    "exit",
                    (if options.HoldToGiveUp.Value then ignore else give_up),
                    (if options.HoldToGiveUp.Value then give_up else ignore)
                )
                |* HotkeyListener("offset", fun () -> if not (liveplay :> IReplayProvider).Finished then change_offset this.State)

            override this.OnEnter(previous) =
                let now = Timestamp.now ()
                if previous <> ScreenType.Play then
                    CURRENT_SESSION.PlaysStarted <- CURRENT_SESSION.PlaysStarted + 1
                Stats.save_current_session now Content.UserData
                info.SaveData.LastPlayed <- now
                Toolbar.hide_cursor ()

                base.OnEnter(previous)

                if SHOW_START_OVERLAY then
                    Background.dim 0.6f
                    Background.set_parallax_amount 240.0f

                DiscordRPC.playing_timed ("Playing", info.ChartMeta.Title, info.ChartMeta.Length / SelectedChart.rate.Value)

            override this.OnExit(next) =
                CURRENT_SESSION.AddPlaytime info.WithMods.Keys play_time
                if not offset_manually_changed then
                    LocalOffset.automatic this.State info.SaveData options.AutoCalibrateOffset.Value
                Toolbar.show_cursor ()
                Background.set_parallax_amount 40.0f
                Song.set_low_pass 0.0f

                base.OnExit(next)

            override this.Init(parent) =
                base.Init(parent)
                start_overlay.Init this

            override this.Update(elapsed_ms, moved) =
                play_time <- play_time + elapsed_ms
                base.Update(elapsed_ms, moved)
                let now = Song.time_with_offset ()
                let chart_time = now - first_note

                if not (liveplay :> IReplayProvider).Finished && fade_in.Target = 1.0f then
                    Input.pop_gameplay now binds (
                        fun column time is_release ->
                            if is_release then
                                key_state <- Bitmask.unset_key column key_state
                            else
                                key_state <- Bitmask.set_key column key_state

                            liveplay.Add(time, key_state)
                            liveplay_position <- max liveplay_position (time - first_note)
                    )

                    this.State.Scoring.Update liveplay_position
                    liveplay_position <- max liveplay_position chart_time

                    if options.EnablePacemakerFailMidway.Value && PacemakerState.pacemaker_failed scoring pacemaker_state then fail_midway this
                    if this.State.Scoring.Finished then finish_play this

                if fade_in.Value < 1.0f then
                    start_overlay.Update(elapsed_ms, moved)
                    fade_in.Update elapsed_ms

            override this.Draw() =

                if fade_in.Value < 1.0f then
                    let old_m = Render.alpha_multiplier_begin fade_in.Value
                    base.Draw()
                    Render.alpha_multiplier_restore old_m
                    start_overlay.Draw()
                else
                    base.Draw()

                if
                    this.State.CurrentChartTime() < 0.0f<ms>
                    && Song.playing ()
                then
                    Text.draw_b (
                        Style.font,
                        (
                            [(%%"offset").ToString(); sprintf "%.0fms" offset_setting.Value] %> "play.localoffset.hint"
                        ),
                        20.0f,
                        this.Bounds.Left + 20.0f,
                        this.Bounds.Top + 20.0f,
                        Colors.text_subheading
                    )
        }