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
open Interlude.UI.Menu
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
            Metrics.create ruleset info.WithMods.Keys liveplay info.WithMods.Notes SelectedChart.rate.Value

        let binds = options.GameplayBinds.[info.WithMods.Keys - 3]
        let mutable key_state = 0us

        scoring.OnHit.Add(fun h ->
            match h.Guts with
            | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
            | _ -> ()
        )

        let pacemaker_state = PacemakerState.create info pacemaker_ctx

        let mutable recommended_offset = 0.0f

        let offset_setting = LocalAudioSync.offset_setting info.SaveData

        let retry () =
            if
                Screen.change_new
                    (fun () -> play_screen (info, pacemaker_ctx) :> Screen.T)
                    Screen.Type.Play
                    Transitions.EnterGameplay
            then
                Stats.session.PlaysRetried <- Stats.session.PlaysRetried + 1
                
        
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

        let offset_slideout (screen: IPlayScreen) =

            let offset_slider =
                Slider(
                    offset_setting
                    |> Setting.map float32 (fun x -> x * 1.0f<ms>)
                    |> Setting.bound -200.0f 200.0f,
                    Step = 1f,
                    Format = (fun v -> sprintf "%.0fms" v),
                    Position = Position.SliceTop(60.0f).SliceLeft(600.0f)
                )

            Slideout(
                SlideoutContent(offset_slider, 100.0f)
                |+ Text(
                    (fun () ->
                        [(%%"accept_suggestion").ToString(); (sprintf "%.0fms" recommended_offset)] %> "play.localoffset.accept_hint"
                    ),
                    Position = Position.Box(0.0f, 0.0f, 0.0f, 60.0f, 600.0f, 40.0f)
                )
                |+ HotkeyAction(
                    "accept_suggestion",
                    fun () ->
                        offset_setting.Value <- recommended_offset * 1.0f<ms>
                        retry ()
                )
                |+ Callout.frame
                    (Callout.Small
                        .Icon(Icons.INFO)
                        .Body(
                            %"play.localoffset.hint_ii"
                        ))
                    (fun (w, h) -> Position.SliceRight(w)),
                OnOpen =
                    (fun () ->
                        liveplay.Finish()
                        Song.pause ()
                        recommended_offset <- LocalAudioSync.get_automatic screen.State info.SaveData |> float32
                        offset_slider.Select false
                    ),
                OnClose = retry,
                AutoCloseWhen = (fun (_: SlideoutContent) -> not offset_slider.Selected)
            )

        { new IPlayScreen(info.Chart, info.WithColors, pacemaker_state, scoring) with
            override this.AddWidgets() =
                let hud_config = Content.NoteskinConfig.HUD
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

                let offset_slideout = offset_slideout this

                this
                |+ HotkeyHoldAction(
                    "retry",
                    (if options.HoldToGiveUp.Value then ignore else retry),
                    (if options.HoldToGiveUp.Value then retry else ignore)
                )
                |+ HotkeyHoldAction(
                    "exit",
                    (if options.HoldToGiveUp.Value then ignore else give_up),
                    (if options.HoldToGiveUp.Value then give_up else ignore)
                )
                |+ HotkeyAction("offset", offset_slideout.Open)
                |* offset_slideout

            override this.OnEnter(previous) =
                if previous <> Screen.Type.Play then
                    Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1
                info.SaveData.LastPlayed <- Timestamp.now ()
                Toolbar.hide_cursor ()

                base.OnEnter(previous)

                DiscordRPC.playing_timed ("Playing", info.CacheInfo.Title, info.CacheInfo.Length / SelectedChart.rate.Value)

            override this.OnExit(next) =
                if options.AutoCalibrateOffset.Value && recommended_offset = 0.0f then
                    LocalAudioSync.apply_automatic this.State info.SaveData
                Toolbar.show_cursor ()

                base.OnExit(next)

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

            override this.Draw() =
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