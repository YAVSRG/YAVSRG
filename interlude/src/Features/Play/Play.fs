namespace Interlude.Features.Play

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Gameplay.Metrics
open Prelude.Data.Scores
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Web.Shared
open Interlude.Features
open Interlude.Features.Stats
open Interlude.Features.Online
open Interlude.Features.Play.HUD
open Interlude.Features.Score

[<RequireQualifiedAccess>]
type PacemakerMode =
    | None
    | Score of rate: float32 * ReplayData
    | Setting

module PlayScreen =

    let rec play_screen (pacemaker_mode: PacemakerMode) =

        let chart = Gameplay.Chart.WITH_MODS.Value
        let ruleset = Rulesets.current
        let first_note = chart.Notes.[0].Time
        let liveplay = LiveReplayProvider first_note

        let scoring = create ruleset chart.Keys liveplay chart.Notes Gameplay.rate.Value

        let pacemaker_info =
            match pacemaker_mode with
            | PacemakerMode.None -> PacemakerInfo.None
            | PacemakerMode.Score(rate, replay) ->
                let replay_data = StoredReplayProvider(replay) :> IReplayProvider
                let scoring = create ruleset chart.Keys replay_data chart.Notes rate
                PacemakerInfo.Replay scoring
            | PacemakerMode.Setting ->
                let setting =
                    if options.Pacemakers.ContainsKey Rulesets.current_hash then
                        options.Pacemakers.[Rulesets.current_hash]
                    else
                        Pacemaker.Default

                match setting with
                | Pacemaker.Accuracy acc -> PacemakerInfo.Accuracy acc
                | Pacemaker.Lamp lamp ->
                    let l = Rulesets.current.Grading.Lamps.[lamp]
                    PacemakerInfo.Judgement(l.Judgement, l.JudgementThreshold)

        let pacemaker_met (state: PlayState) =
            match state.Pacemaker with
            | PacemakerInfo.None -> true
            | PacemakerInfo.Accuracy x -> scoring.Value >= x
            | PacemakerInfo.Replay r ->
                r.Update Time.infinity
                scoring.Value >= r.Value
            | PacemakerInfo.Judgement(judgement, count) ->
                let actual =
                    if judgement = -1 then
                        scoring.State.ComboBreaks
                    else
                        let mutable c = scoring.State.Judgements.[judgement]

                        for j = judgement + 1 to scoring.State.Judgements.Length - 1 do
                            if scoring.State.Judgements.[j] > 0 then
                                c <- 1000000

                        c

                actual <= count

        let binds = options.GameplayBinds.[chart.Keys - 3]
        let mutable key_state = 0us

        scoring.OnHit.Add(fun h ->
            match h.Guts with
            | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
            | _ -> ()
        )

        { new IPlayScreen(chart, pacemaker_info, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x =
                    add_widget (this, this.Playfield, this.State) x

                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter
                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget Pacemaker
                add_widget JudgementCounts
                add_widget JudgementMeter
                add_widget EarlyLateMeter
                add_widget RateModMeter
                add_widget BPMMeter

            override this.OnEnter(previous) =
                if previous <> Screen.Type.Play then
                    Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1

                base.OnEnter(previous)

                DiscordRPC.playing_timed (
                    "Playing",
                    Gameplay.Chart.CACHE_DATA.Value.Title,
                    Gameplay.Chart.CACHE_DATA.Value.Length / Gameplay.rate.Value
                )

            override this.OnExit(next) =
                if next = Screen.Type.Score then
                    Stats.session.PlaysCompleted <- Stats.session.PlaysCompleted + 1
                elif next = Screen.Type.Play then
                    Stats.session.PlaysRetried <- Stats.session.PlaysRetried + 1
                else
                    Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1

                if options.AutoCalibrateOffset.Value then
                    AutomaticSync.apply (scoring)

                base.OnExit(next)

            override this.Update(elapsed_ms, moved) =
                Stats.session.PlayTime <- Stats.session.PlayTime + elapsed_ms
                base.Update(elapsed_ms, moved)
                let now = Song.time_with_offset ()
                let chart_time = now - first_note

                if not (liveplay :> IReplayProvider).Finished then
                    // feed keyboard input into the replay provider
                    Input.pop_gameplay (
                        binds,
                        fun column time is_release ->
                            if time > now then
                                Logging.Debug("Received input event from the future")
                            else
                                if is_release then
                                    key_state <- Bitmask.unset_key column key_state
                                else
                                    key_state <- Bitmask.set_key column key_state

                                liveplay.Add(time, key_state)
                    )

                    this.State.Scoring.Update chart_time

                if (%%"retry").Tapped() then
                    Screen.change_new
                        (fun () -> play_screen (pacemaker_mode) :> Screen.T)
                        Screen.Type.Play
                        Transitions.Flags.Default
                    |> ignore

                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then
                    liveplay.Finish()

                    Screen.change_new
                        (fun () ->
                            let sd =
                                ScoreInfoProvider(
                                    Gameplay.make_score (
                                        (liveplay :> IReplayProvider).GetFullReplay(),
                                        this.Chart.Keys
                                    ),
                                    Gameplay.Chart.CHART.Value,
                                    this.State.Ruleset,
                                    ModChart = Gameplay.Chart.WITH_MODS.Value,
                                    Difficulty = Gameplay.Chart.RATING.Value
                                )

                            (sd, Gameplay.set_score (pacemaker_met this.State) sd) |> ScoreScreen
                        )
                        Screen.Type.Score
                        Transitions.Flags.Default
                    |> ignore

            override this.Draw() =
                base.Draw()

                if options.AutoCalibrateOffset.Value && this.State.CurrentChartTime() < 0.0f<ms> then
                    Text.draw_b (
                        Style.font,
                        sprintf "Local offset: %.0fms" AutomaticSync.offset.Value,
                        20.0f,
                        this.Bounds.Left + 20.0f,
                        this.Bounds.Top + 20.0f,
                        Colors.text_subheading
                    )
        }

    let multiplayer_screen () =

        let chart = Gameplay.Chart.WITH_MODS.Value
        let ruleset = Rulesets.current
        let first_note = chart.Notes.[0].Time
        let liveplay = LiveReplayProvider first_note

        let scoring = create ruleset chart.Keys liveplay chart.Notes Gameplay.rate.Value

        let binds = options.GameplayBinds.[chart.Keys - 3]
        let mutable key_state = 0us
        let mutable packet_count = 0

        Lobby.start_playing ()
        Gameplay.Online.Multiplayer.add_own_replay (scoring, liveplay)

        scoring.OnHit.Add(fun h ->
            match h.Guts with
            | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
            | _ -> ()
        )

        let send_replay_packet (now: Time) =
            if not (liveplay :> IReplayProvider).Finished then
                liveplay.Add(now, key_state)

            use ms = new System.IO.MemoryStream()
            use bw = new System.IO.BinaryWriter(ms)
            liveplay.ExportLiveBlock bw
            Lobby.play_data (ms.ToArray())
            packet_count <- packet_count + 1

        { new IPlayScreen(chart, PacemakerInfo.None, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x =
                    add_widget (this, this.Playfield, this.State) x

                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget ComboMeter
                add_widget ProgressMeter
                add_widget Pacemaker
                add_widget JudgementCounts
                add_widget JudgementMeter
                add_widget EarlyLateMeter
                add_widget RateModMeter
                add_widget BPMMeter
                add_widget MultiplayerScoreTracker

            override this.OnEnter(previous) =
                Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1
                base.OnEnter(previous)

            override this.OnExit(next) =
                if next = Screen.Type.Score then
                    Stats.session.PlaysCompleted <- Stats.session.PlaysCompleted + 1
                else
                    Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1

                if options.AutoCalibrateOffset.Value then
                    AutomaticSync.apply (scoring)

                if next <> Screen.Type.Score then
                    Lobby.abandon_play ()

                base.OnExit(next)

                DiscordRPC.playing_timed (
                    "Multiplayer",
                    Gameplay.Chart.CACHE_DATA.Value.Title,
                    Gameplay.Chart.CACHE_DATA.Value.Length / Gameplay.rate.Value
                )

            override this.Update(elapsed_ms, moved) =
                Stats.session.PlayTime <- Stats.session.PlayTime + elapsed_ms
                base.Update(elapsed_ms, moved)
                let now = Song.time_with_offset ()
                let chart_time = now - first_note

                if not (liveplay :> IReplayProvider).Finished then

                    if chart_time / MULTIPLAYER_REPLAY_DELAY_MS / 1.0f<ms> |> floor |> int > packet_count then
                        send_replay_packet (now)

                    Input.pop_gameplay (
                        binds,
                        fun column time is_release ->
                            if time > now then
                                Logging.Debug("Received input event from the future")
                            else
                                if is_release then
                                    key_state <- Bitmask.unset_key column key_state
                                else
                                    key_state <- Bitmask.set_key column key_state

                                liveplay.Add(time, key_state)
                    )

                    this.State.Scoring.Update chart_time

                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then
                    liveplay.Finish()
                    send_replay_packet (now)
                    Lobby.finish_playing ()

                    Screen.change_new
                        (fun () ->
                            let sd =
                                ScoreInfoProvider(
                                    Gameplay.make_score (
                                        (liveplay :> IReplayProvider).GetFullReplay(),
                                        this.Chart.Keys
                                    ),
                                    Gameplay.Chart.CHART.Value,
                                    this.State.Ruleset,
                                    ModChart = Gameplay.Chart.WITH_MODS.Value,
                                    Difficulty = Gameplay.Chart.RATING.Value
                                )

                            (sd, Gameplay.set_score true sd) |> ScoreScreen
                        )
                        Screen.Type.Score
                        Transitions.Flags.Default
                    |> ignore
        }
