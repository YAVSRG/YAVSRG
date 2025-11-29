namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.UI
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Data.User.Stats
open Interlude.Options
open Interlude.UI
open Interlude.Content
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Play.Practice

type PracticeScreen =

    static let UNPAUSE_NOTE_LEADWAY = 800.0f<ms / rate>

    static member Create(info: LoadedChartInfo, start_at: Time) : Screen =

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
                PracticePoint = start_at |> Setting.bounded (0.0f<ms>, last_allowed_practice_point)
            }

        let FIRST_NOTE = info.WithMods.FirstNote

        let reset_to_practice_point () =
            liveplay <- LiveReplay FIRST_NOTE

            scoring <-
                ScoreProcessor.create Rulesets.current info.WithMods.Keys liveplay info.WithMods.Notes SelectedChart.rate.Value

            let ignore_notes_before_time : Time = state.PracticePoint.Value + UNPAUSE_NOTE_LEADWAY * SelectedChart.rate.Value
            scoring.IgnoreNotesBefore ignore_notes_before_time

            scoring.OnEvent.Add(fun h ->
                match h.Action with
                | Hit d
                | Hold d when not d.Missed -> CURRENT_SESSION.NotesHit <- CURRENT_SESSION.NotesHit + 1
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

        let paused_overlay (screen: IPlayScreen) =
            PracticeControls(
                state,
                info.WithMods,
                fun t ->
                    state.PracticePoint.Set t
                    Song.seek t
                    resume_from_current_place <- false
            )

        { new IPlayScreen(info, PacemakerState.None, scoring, HudContextInner.Play) with
            override this.Init(parent: Widget) =
                this.Add (paused_overlay this)
                base.Init(parent)

            override this.OnEnter(p) =
                base.OnEnter(p)
                Song.seek state.PracticePoint.Value
                Song.pause ()
                DiscordRPC.playing ("Practice mode", info.ChartMeta.Title)

            override this.OnBack() =
                Song.resume ()
                base.OnBack()

            override this.Update(elapsed_ms, moved) =
                let now = Song.time_with_offset ()
                let chart_time = now - FIRST_NOTE

                if (%%"retry").Pressed() then
                    restart this

                elif (%%"offset").Pressed() then
                    if not state.Paused.Value then pause this
                    LocalOffsetPage(
                        (match state.SyncSuggestions with Some s -> s.AudioOffset | None -> state.SaveData.Offset),
                        LocalOffset.offset_setting state.SaveData
                    )
                        .WithOnClose(fun () -> restart this)
                        .Show()

                elif (%%"accept_offset").Pressed() then
                    if state.Paused.Value then
                        PracticeState.accept_suggested_offset state
                    else
                        pause this
                        PracticeState.accept_suggested_offset state
                        restart this

                elif (%%"reset_offset").Pressed() then
                    if state.Paused.Value then
                        PracticeState.reset_offset state
                    else
                        pause this
                        PracticeState.reset_offset state
                        restart this

                elif state.Paused.Value then
                    if (%%"pause").Pressed() then
                        resume this
                    else
                        SelectedChart.change_rate_hotkeys (fun change_by -> SelectedChart.rate.Value <- SelectedChart.rate.Value + change_by)

                elif (%%"exit").Pressed() then
                    if not state.Paused.Value then
                        pause this
                        input_key_state <- 0us
                    else
                        Screen.back Transitions.Default |> ignore

                elif not (liveplay :> IReplay).Finished then
                    Input.pop_gameplay now binds (
                        fun column time is_release ->
                            if is_release then
                                input_key_state <- Bitmask.unset_key column input_key_state
                            else
                                input_key_state <- Bitmask.set_key column input_key_state

                            liveplay.AddFrame(time, input_key_state)
                    )

                    this.State.Scoring.Update chart_time

                if not state.Paused.Value then
                    CURRENT_SESSION.PracticeTime <- CURRENT_SESSION.PracticeTime + elapsed_ms
                    Input.finish_frame_events()

                base.Update(elapsed_ms, moved)

                if this.State.Scoring.Finished && not state.Paused.Value then
                    pause this
        }