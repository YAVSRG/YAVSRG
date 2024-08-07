﻿namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Interlude.Options
open Interlude.UI
open Interlude.Content
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Stats
open Interlude.Features.Play.HUD
open Interlude.Features.Play.Practice

module PracticeScreen =

    let UNPAUSE_NOTE_LEADWAY = 800.0f<ms>

    type ControlOverlay(state: PracticeState, with_mods, on_seek) =
        inherit SlideContainer(NodeType.None)

        let mutable show = true

        let info_callout =
            Callout.Small
                .Icon(Icons.TARGET)
                .Title(%"practice.info.title")
                .Hotkey(%"practice.info.play", "skip")
                .Hotkey(%"practice.info.restart", "retry")
                .Hotkey(%"practice.info.options", "exit")
                .Hotkey(%"practice.info.accept_suggestion", "accept_suggestion")

        let sync_controls = SyncSuggestionControls state
        let slideout = Slideout(sync_controls, AutoCloseWhen = K false)

        override this.Init(parent) =
            this
            |+ Timeline(with_mods, on_seek, SelectedChart.rate)
            |+ (Callout.frame
                    info_callout
                    (fun (w, h) -> Position.Box(0.0f, 1.0f, 20.0f, -100.0f - h, w + 100.0f, h))
                ).Conditional(fun () -> show)
            |* slideout

            base.Init parent

            slideout.Open()

        override this.Update(elapsed_ms, moved) =
            base.Update(elapsed_ms, moved)

            if state.Paused.Value && not show then
                show <- true
                slideout.Open()
                this.Position <- Position.DEFAULT
                Toolbar.show_cursor ()
            elif not state.Paused.Value && show then
                show <- false
                slideout.Close()

                this.Position <-
                    { Position.DEFAULT with
                        Bottom = 1.0f %+ 100.0f
                    }
                Toolbar.hide_cursor ()

            if show && not sync_controls.Focused then
                Screen.back Transitions.Default |> ignore

    let practice_screen (info: LoadedChartInfo, start_at: Time) =

        let mutable liveplay = Unchecked.defaultof<_>
        let mutable scoring = Unchecked.defaultof<_>
        let mutable resume_from_current_place = false

        let last_allowed_practice_point =
            info.WithMods.LastNote - 5.0f<ms> - Song.LEADIN_TIME * SelectedChart.rate.Value

        let state: PracticeState =
            {
                Chart = info.Chart
                SaveData = info.SaveData
                Paused = Setting.simple true
                SyncMode = Setting.simple SyncMode.AUDIO_OFFSET
                SyncSuggestions = None
                PracticePoint = Setting.bounded start_at 0.0f<ms> last_allowed_practice_point
            }

        let FIRST_NOTE = info.WithMods.FirstNote

        let reset_to_practice_point () =
            liveplay <- LiveReplayProvider FIRST_NOTE

            scoring <-
                ScoreProcessor.create Rulesets.current info.WithMods.Keys liveplay info.WithMods.Notes SelectedChart.rate.Value

            let ignore_notes_before_time =
                state.PracticePoint.Value + UNPAUSE_NOTE_LEADWAY * SelectedChart.rate.Value

            let mutable i = 0

            while i < scoring.HitData.Length
                  && let struct (t, _, _) = scoring.HitData.[i] in
                     t < ignore_notes_before_time do
                let struct (_, deltas, flags) = scoring.HitData.[i]

                for k = 0 to info.WithMods.Keys - 1 do
                    flags.[k] <- HitStatus.HIT_ACCEPTED
                    deltas.[k] <- -Time.infinity

                i <- i + 1

            scoring.OnHit.Add(fun h ->
                match h.Guts with
                | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
                | _ -> ()
            )

        do reset_to_practice_point ()

        let binds = options.GameplayBinds.[info.WithMods.Keys - 3]
        let mutable input_key_state = 0us

        let restart (screen: IPlayScreen) =
            reset_to_practice_point ()
            screen.State.ChangeScoring scoring
            Song.play_from state.PracticePoint.Value
            state.Paused.Set false

        let pause (_: IPlayScreen) =
            Song.pause ()
            state.Paused.Set true
            PracticeState.update_suggestions scoring state
            resume_from_current_place <- true

        let resume (screen: IPlayScreen) =
            if not scoring.Finished && resume_from_current_place then
                Song.resume ()
                state.Paused.Set false
            else
                restart screen

        let paused_overlay =
            ControlOverlay(
                state,
                info.WithMods,
                fun t ->
                    state.PracticePoint.Set t
                    Song.seek t
                    resume_from_current_place <- false
            )

        { new IPlayScreen(info.Chart, info.WithColors, PacemakerState.None, scoring) with
            override this.AddWidgets() =

                let hud_config = Content.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, hud_config) position constructor

                if hud_config.ComboEnabled then add_widget hud_config.ComboPosition Combo
                if hud_config.ProgressMeterEnabled then add_widget hud_config.ProgressMeterPosition ProgressMeter
                if hud_config.AccuracyEnabled then add_widget hud_config.AccuracyPosition Accuracy
                if hud_config.TimingDisplayEnabled then add_widget hud_config.TimingDisplayPosition TimingDisplay
                if hud_config.JudgementCounterEnabled then add_widget hud_config.JudgementCounterPosition JudgementCounter
                if hud_config.JudgementMeterEnabled then add_widget hud_config.JudgementMeterPosition JudgementMeter
                if hud_config.EarlyLateMeterEnabled then add_widget hud_config.EarlyLateMeterPosition EarlyLateMeter
                if hud_config.RateModMeterEnabled then add_widget hud_config.RateModMeterPosition RateModMeter
                if hud_config.BPMMeterEnabled then add_widget hud_config.BPMMeterPosition BPMMeter
                if hud_config.InputMeterEnabled then add_widget hud_config.InputMeterPosition InputMeter
                if hud_config.CustomImageEnabled then add_widget hud_config.CustomImagePosition CustomImage

                this.Add paused_overlay

            override this.OnEnter(p) =
                base.OnEnter(p)
                Song.seek state.PracticePoint.Value
                Song.pause ()
                DiscordRPC.playing ("Practice mode", info.CacheInfo.Title)

            override this.OnBack() =
                Song.resume ()
                base.OnBack()

            override this.Update(elapsed_ms, moved) =
                let now = Song.time_with_offset ()
                let chart_time = now - FIRST_NOTE

                if (%%"retry").Tapped() then
                    restart this

                elif (%%"accept_suggestion").Tapped() then
                    if state.Paused.Value then
                        PracticeState.accept_suggestion state
                    else
                        pause this
                        PracticeState.accept_suggestion state
                        restart this

                elif state.Paused.Value then
                    if (%%"skip").Tapped() then
                        resume this
                    else
                        SelectedChart.change_rate_hotkeys (fun change_by -> SelectedChart.rate.Value <- SelectedChart.rate.Value + change_by)

                elif (%%"exit").Tapped() then
                    pause this
                    input_key_state <- 0us

                elif not (liveplay :> IReplayProvider).Finished then
                    Input.pop_gameplay now binds (
                        fun column time is_release ->
                            if is_release then
                                input_key_state <- Bitmask.unset_key column input_key_state
                            else
                                input_key_state <- Bitmask.set_key column input_key_state

                            liveplay.Add(time, input_key_state)
                    )

                    this.State.Scoring.Update chart_time

                if not state.Paused.Value then
                    Stats.session.PracticeTime <- Stats.session.PracticeTime + elapsed_ms
                    Input.finish_frame_events()

                base.Update(elapsed_ms, moved)

                if this.State.Scoring.Finished && not state.Paused.Value then
                    pause this
        }
