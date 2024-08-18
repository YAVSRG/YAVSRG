namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Charts.Processing
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Stats
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.Play.HUD

module PlayScreen =

    let rec play_screen (info: LoadedChartInfo, pacemaker_ctx: PacemakerCreationContext) =

        let ruleset = Rulesets.current
        let first_note = info.WithMods.FirstNote
        let liveplay = LiveReplayProvider first_note

        let scoring =
            ScoreProcessor.create ruleset info.WithMods.Keys liveplay info.WithMods.Notes SelectedChart.rate.Value

        let binds = options.GameplayBinds.[info.WithMods.Keys - 3]
        let mutable key_state = 0us

        scoring.OnHit.Add(fun h ->
            match h.Guts with
            | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
            | _ -> ()
        )

        let pacemaker_state = PacemakerState.create info pacemaker_ctx

        let mutable offset_manually_changed = false
        let offset_setting = LocalOffset.offset_setting info.SaveData

        let retry () =
            if
                Screen.change_new
                    (fun () -> play_screen (info, pacemaker_ctx) :> Screen.T)
                    Screen.Type.Play
                    Transitions.EnterGameplay
            then
                Stats.session.PlaysRetried <- Stats.session.PlaysRetried + 1

        let SHOW_START_OVERLAY = true
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
            if Gameplay.continue_endless_mode() then Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1
        
        let give_up () =
            let is_giving_up_play = not (liveplay :> IReplayProvider).Finished && (Song.time() - first_note) * SelectedChart.rate.Value > 15000f<ms>

            if 
                if is_giving_up_play then
                    liveplay.Finish()
                    scoring.Update Time.infinity
                    Screen.change_new
                        (fun () ->
                            let score_info =
                                Gameplay.score_info_from_gameplay
                                    info
                                    scoring
                                    ((liveplay :> IReplayProvider).GetFullReplay())
                            ScoreScreen(score_info, ImprovementFlags.None, true)
                        )
                        Screen.Type.Score
                        Transitions.LeaveGameplay
                else
                    Screen.back Transitions.LeaveGameplay
            then
                Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1

        let finish_play() =
            liveplay.Finish()
            if
                Screen.change_new
                    (fun () ->
                        let score_info =
                            Gameplay.score_info_from_gameplay
                                info
                                scoring
                                ((liveplay :> IReplayProvider).GetFullReplay())

                        (score_info, Gameplay.set_score (PacemakerState.pacemaker_met scoring pacemaker_state) score_info info.SaveData, true)
                        |> ScoreScreen
                    )
                    Screen.Type.Score
                    Transitions.EnterGameplay
            then
                Stats.session.PlaysCompleted <- Stats.session.PlaysCompleted + 1

        let change_offset(state) =
            Song.pause()
            liveplay.Finish()
            offset_manually_changed <- true
            LocalOffsetPage(state, info.SaveData, offset_setting, retry).Show()

        { new IPlayScreen(info.Chart, info.WithColors, pacemaker_state, scoring) with
            override this.AddWidgets() =
                let hud_config = Content.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, hud_config) position constructor

                if hud_config.ComboEnabled then add_widget hud_config.ComboPosition Combo
                if hud_config.SkipButtonEnabled then add_widget hud_config.SkipButtonPosition SkipButton
                if hud_config.ProgressMeterEnabled then add_widget hud_config.ProgressMeterPosition ProgressMeter
                if hud_config.AccuracyEnabled then add_widget hud_config.AccuracyPosition Accuracy
                if hud_config.TimingDisplayEnabled then add_widget hud_config.TimingDisplayPosition TimingDisplay
                if this.State.Pacemaker <> PacemakerState.None then add_widget hud_config.PacemakerPosition Pacemaker
                if hud_config.JudgementCounterEnabled then add_widget hud_config.JudgementCounterPosition JudgementCounter
                if hud_config.JudgementMeterEnabled then add_widget hud_config.JudgementMeterPosition JudgementMeter
                if hud_config.EarlyLateMeterEnabled then add_widget hud_config.EarlyLateMeterPosition EarlyLateMeter
                if hud_config.RateModMeterEnabled then add_widget hud_config.RateModMeterPosition RateModMeter
                if hud_config.BPMMeterEnabled then add_widget hud_config.BPMMeterPosition BPMMeter
                if hud_config.InputMeterEnabled then add_widget hud_config.InputMeterPosition InputMeter
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
                //|+ HotkeyAction("skip", fun () -> this |* FailOverlay())
                |* HotkeyAction("offset", fun () -> change_offset this.State)

            override this.OnEnter(previous) =
                if previous <> Screen.Type.Play then
                    Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1
                info.SaveData.LastPlayed <- Timestamp.now ()
                Toolbar.hide_cursor ()

                base.OnEnter(previous)

                if SHOW_START_OVERLAY then
                    Background.dim 0.6f
                    Background.set_parallax_amount 240.0f

                DiscordRPC.playing_timed ("Playing", info.CacheInfo.Title, info.CacheInfo.Length / SelectedChart.rate.Value)

            override this.OnExit(next) =
                if options.AutoCalibrateOffset.Value && not offset_manually_changed then
                    LocalOffset.apply_automatic this.State info.SaveData
                Toolbar.show_cursor ()
                Background.set_parallax_amount 40.0f

                base.OnExit(next)

            override this.Init(parent) =
                base.Init(parent)
                start_overlay.Init this

            override this.Update(elapsed_ms, moved) =
                Stats.session.PlayTime <- Stats.session.PlayTime + elapsed_ms
                base.Update(elapsed_ms, moved)
                let now = Song.time_with_offset ()
                let chart_time = now - first_note

                if not (liveplay :> IReplayProvider).Finished then
                    Input.pop_gameplay now binds (
                        fun column time is_release ->
                            if is_release then
                                key_state <- Bitmask.unset_key column key_state
                            else
                                key_state <- Bitmask.set_key column key_state

                            liveplay.Add(time, key_state)
                    )

                    this.State.Scoring.Update chart_time

                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then finish_play()

                if fade_in.Value < 1.0f then
                    start_overlay.Update(elapsed_ms, moved)
                    fade_in.Update elapsed_ms

            override this.Draw() =
                
                if fade_in.Value < 1.0f then
                    let old_m = Alpha.change_multiplier fade_in.Value
                    base.Draw()
                    Alpha.change_multiplier old_m |> ignore
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
                            if options.AutoCalibrateOffset.Value then [sprintf "%.0fms" offset_setting.Value] %> "play.localoffset.automatic"
                            else [(%%"offset").ToString()] %> "play.localoffset.hint"
                        ),
                        20.0f,
                        this.Bounds.Left + 20.0f,
                        this.Bounds.Top + 20.0f,
                        Colors.text_subheading
                    )
        }