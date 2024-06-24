namespace Interlude.Features.Play


open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Charts.Processing
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Web.Shared
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Stats
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.Play.HUD

module PlayScreenMultiplayer =

    let multiplayer_screen (info: LoadedChartInfo, lobby: Lobby) =

        let ruleset = Rulesets.current
        let first_note = info.WithMods.FirstNote
        let liveplay = LiveReplayProvider first_note

        let scoring =
            Metrics.create ruleset info.WithMods.Keys liveplay info.WithMods.Notes SelectedChart.rate.Value

        let binds = options.GameplayBinds.[info.WithMods.Keys - 3]
        let mutable key_state = 0us
        let mutable packet_count = 0

        lobby.StartPlaying()
        lobby.AddReplayInfo(
            Network.credentials.Username,
            { 
                Replay = liveplay
                ScoreMetric = scoring
                GetScoreInfo = fun () ->
                    if not (liveplay :> IReplayProvider).Finished then
                        liveplay.Finish()

                    scoring.Update Time.infinity

                    let replay_data = (liveplay :> IReplayProvider).GetFullReplay()

                    {
                        CachedChart = info.CacheInfo
                        Chart = info.Chart
                        WithMods = info.WithMods

                        PlayedBy = Data.ScorePlayedBy.You
                        TimePlayed = Timestamp.now ()
                        Rate = SelectedChart.rate.Value

                        Replay = replay_data
                        Scoring = scoring
                        Lamp = Lamp.calculate scoring.Ruleset.Grading.Lamps scoring.State
                        Grade = Grade.calculate scoring.Ruleset.Grading.Grades scoring.State

                        Rating = info.Rating
                        Patterns = info.Patterns
                        Physical = Performance.calculate info.Rating info.WithMods.Keys scoring |> fst

                        ImportedFromOsu = false
                    }
            }
        )

        scoring.OnHit.Add(fun h ->
            match h.Guts with
            | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
            | _ -> ()
        )

        let send_replay_packet (chart_time: ChartTime) =
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)
            liveplay.ExportLiveBlock bw
            lobby.SendReplayData(float32 chart_time, ms.ToArray())
            packet_count <- packet_count + 1

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
                lobby.AbandonPlaying()
                Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1

        let finish_play(chart_time: ChartTime) =
            liveplay.Finish()
            send_replay_packet chart_time
            lobby.FinishPlaying()
            if
                Screen.change_new
                    (fun () ->
                        let score_info =
                            Gameplay.score_info_from_gameplay
                                info
                                scoring
                                ((liveplay :> IReplayProvider).GetFullReplay())

                        (score_info, Gameplay.set_score true score_info info.SaveData, true)
                        |> ScoreScreen
                    )
                    Screen.Type.Score
                    Transitions.EnterGameplay
            then
                Stats.session.PlaysCompleted <- Stats.session.PlaysCompleted + 1

        { new IPlayScreen(info.Chart, info.WithColors, PacemakerState.None, scoring) with
            override this.AddWidgets() =
                let hud_config = Content.NoteskinConfig.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, hud_config) position constructor

                if hud_config.ComboEnabled then add_widget hud_config.ComboPosition Combo
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
                add_widget hud_config.MultiplayerScoreTrackerPosition
                    (fun (config, state) -> MultiplayerScoreTracker(config, state, lobby.Replays))

                this
                |* HotkeyHoldAction(
                    "exit",
                    (if options.HoldToGiveUp.Value then ignore else give_up),
                    (if options.HoldToGiveUp.Value then give_up else ignore)
                )

            override this.OnEnter(previous) =
                Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1
                info.SaveData.LastPlayed <- Timestamp.now ()
                Toolbar.hide_cursor ()

                base.OnEnter(previous)

                DiscordRPC.playing_timed (
                    "Multiplayer",
                    info.CacheInfo.Title,
                    info.CacheInfo.Length / SelectedChart.rate.Value
                )

            override this.OnExit(next) =
                if options.AutoCalibrateOffset.Value then
                    LocalAudioSync.apply_automatic this.State info.SaveData

                Toolbar.show_cursor ()
                base.OnExit(next)

            override this.Update(elapsed_ms, moved) =
                Stats.session.PlayTime <- Stats.session.PlayTime + elapsed_ms
                base.Update(elapsed_ms, moved)
                let now = Song.time_with_offset ()
                let chart_time : ChartTime = now - first_note

                if not (liveplay :> IReplayProvider).Finished then

                    Input.pop_gameplay now binds (
                        fun column time is_release ->
                            if is_release then
                                key_state <- Bitmask.unset_key column key_state
                            else
                                key_state <- Bitmask.set_key column key_state

                            liveplay.Add(time, key_state)
                    )

                    if chart_time / MULTIPLAYER_REPLAY_DELAY_MS / 1.0f<ms> |> floor |> int > packet_count then
                        send_replay_packet chart_time

                    this.State.Scoring.Update chart_time

                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then finish_play chart_time
        }
