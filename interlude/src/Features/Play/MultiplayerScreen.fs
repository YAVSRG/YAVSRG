namespace Interlude.Features.Play

open System.IO
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay.Replays
open Prelude.Gameplay.Scoring
open Prelude.Skins.HudLayouts
open Prelude.Calculator
open Prelude.Data.User
open Prelude.Data.User.Stats
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Web.Shared
open Interlude.Features.Pacemaker
open Interlude.Features.Gameplay
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.Play.HUD

type MultiplayerScreen =

    static member Create(info: LoadedChartInfo, lobby: Lobby) : Screen =

        let ruleset = Rulesets.current
        let first_note = info.WithMods.FirstNote
        let liveplay = LiveReplay first_note

        let scoring =
            ScoreProcessor.create ruleset info.WithMods.Keys liveplay info.WithMods.Notes SelectedChart.rate.Value

        let binds = options.GameplayBinds.[info.WithMods.Keys - 3]
        let mutable key_state = 0us
        let mutable liveplay_position = -Time.infinity
        let mutable packet_count = 0
        let mutable play_time = 0.0

        let mutable quit_out_early = false

        lobby.StartPlaying()
        lobby.AddReplayInfo(
            Network.credentials.Username,
            {
                Replay = liveplay
                ScoreProcessor = scoring
                GetScoreInfo = fun () ->
                    if not (liveplay :> IReplay).Finished then
                        liveplay.Finish()

                    scoring.Update Time.infinity

                    let replay_data = (liveplay :> IReplay).GetFullReplay()

                    {
                        ChartMeta = info.ChartMeta
                        Chart = info.Chart
                        WithMods = info.WithMods

                        PlayedBy = ScorePlayedBy.You
                        TimePlayed = Timestamp.now ()
                        Rate = SelectedChart.rate.Value

                        Replay = replay_data
                        Scoring = scoring
                        Lamp = Lamp.calculate scoring.Ruleset.Lamps scoring.JudgementCounts scoring.ComboBreaks
                        Grade = Grade.calculate scoring.Ruleset.Grades scoring.Accuracy

                        Rating = info.Difficulty
                        Performance = Performance.calculate info.Difficulty scoring

                        ImportedFromOsu = false
                        IsFailed = quit_out_early
                    }
            }
        )

        scoring.OnEvent.Add(fun h ->
            match h.Action with
            | Hit d
            | Hold d when not d.Missed -> CURRENT_SESSION.NotesHit <- CURRENT_SESSION.NotesHit + 1
            | _ -> ()
        )

        let send_replay_packet (chart_time: ChartTime) =
            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)
            liveplay.ExportLiveBlock bw
            lobby.SendReplayData(float32 chart_time, ms.ToArray())
            packet_count <- packet_count + 1

        let give_up () =
            let is_giving_up_play = not (liveplay :> IReplay).Finished && (Song.time() - first_note) / SelectedChart.rate.Value > 15000f<ms / rate>
            quit_out_early <- true

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
                                    ((liveplay :> IReplay).GetFullReplay())
                                    quit_out_early
                            ScoreScreen(score_info, (ImprovementFlags.None, None), true)
                        )
                        ScreenType.Score
                        Transitions.LeaveGameplay
                else
                    Screen.back Transitions.LeaveGameplay
            then
                lobby.AbandonPlaying()
                CURRENT_SESSION.PlaysQuit <- CURRENT_SESSION.PlaysQuit + 1

        let finish_play (chart_time: ChartTime) =
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
                                ((liveplay :> IReplay).GetFullReplay())
                                false

                        (score_info, Gameplay.set_score false score_info info.SaveData, true)
                        |> ScoreScreen
                    )
                    ScreenType.Score
                    Transitions.EnterGameplayNoFadeAudio
            then
                CURRENT_SESSION.PlaysCompleted <- CURRENT_SESSION.PlaysCompleted + 1

        { new IPlayScreen(info, PacemakerState.None, scoring) with
            override this.AddWidgets hud_ctx =

                hud_ctx.TryAdd(HudElement.Combo)
                hud_ctx.TryAdd(HudElement.ProgressPie)
                hud_ctx.TryAdd(HudElement.Accuracy)
                hud_ctx.TryAdd(HudElement.ErrorBar)
                hud_ctx.TryAdd(HudElement.ColumnErrorBars)
                hud_ctx.TryAdd(HudElement.Pacemaker)
                hud_ctx.TryAdd(HudElement.JudgementCounter)
                hud_ctx.TryAdd(HudElement.Judgement)
                hud_ctx.TryAdd(HudElement.EarlyLate)
                hud_ctx.TryAdd(HudElement.RateMods)
                hud_ctx.TryAdd(HudElement.BPM)
                hud_ctx.TryAdd(HudElement.InputMeter)
                hud_ctx.TryAdd(HudElement.KeysPerSecond)
                hud_ctx.TryAdd(HudElement.CustomImage)
                if hud_ctx.Config.MultiplayerScoreTrackerPosition.RelativeToPlayfield then hud_ctx.Playfield.Add else hud_ctx.Screen.Add
                <| MultiplayerScoreTracker(hud_ctx, lobby.Replays)

                this
                    .Add(
                        HotkeyHoldAction(
                            "exit",
                            (if options.HoldToGiveUp.Value then ignore else give_up),
                            (if options.HoldToGiveUp.Value then give_up else ignore)
                        )
                    )

            override this.OnEnter(previous) =
                let now = Timestamp.now()
                CURRENT_SESSION.PlaysStarted <- CURRENT_SESSION.PlaysStarted + 1
                Stats.save_current_session now Content.UserData
                info.SaveData.LastPlayed <- now
                Toolbar.hide_cursor ()

                base.OnEnter(previous)

                DiscordRPC.playing_timed (
                    "Multiplayer",
                    info.ChartMeta.Title,
                    info.ChartMeta.Length / SelectedChart.rate.Value
                )

            override this.OnExit(next) =
                CURRENT_SESSION.AddPlaytime info.WithMods.Keys play_time
                LocalOffset.automatic this.State info.SaveData options.AutoCalibrateOffset.Value

                Toolbar.show_cursor ()
                base.OnExit(next)

            override this.Update(elapsed_ms, moved) =
                play_time <- play_time + elapsed_ms
                base.Update(elapsed_ms, moved)
                let now = Song.time_with_offset ()
                let chart_time : ChartTime = now - first_note

                if not (liveplay :> IReplay).Finished then

                    Input.pop_gameplay now binds (
                        fun column time is_release ->
                            if is_release then
                                key_state <- Bitmask.unset_key column key_state
                            else
                                key_state <- Bitmask.set_key column key_state

                            liveplay.AddFrame(time, key_state)
                            liveplay_position <- max liveplay_position (time - first_note)
                    )

                    if chart_time / MULTIPLAYER_REPLAY_DELAY_MS / 1.0f<ms> |> floor |> int > packet_count then
                        send_replay_packet liveplay_position

                    this.State.Scoring.Update liveplay_position
                    liveplay_position <- max liveplay_position chart_time

                if this.State.Scoring.Finished && not (liveplay :> IReplay).Finished then finish_play chart_time
        }