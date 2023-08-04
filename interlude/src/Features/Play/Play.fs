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
open Interlude.Features.Online
open Interlude.Features.Play.HUD
open Interlude.Features.Score

[<RequireQualifiedAccess>]
type PacemakerMode =
    | None
    | Score of rate: float32 * ReplayData
    | Setting

module PlayScreen =
    
    let rec play_screen (pacemakerMode: PacemakerMode) =
        
        let chart = Gameplay.Chart.withMods.Value
        let ruleset = Rulesets.current
        let firstNote = chart.Notes.[0].Time
        let liveplay = LiveReplayProvider firstNote
        let scoring = createScoreMetric ruleset chart.Keys liveplay chart.Notes Gameplay.rate.Value

        let pacemakerInfo =
            match pacemakerMode with
            | PacemakerMode.None -> PacemakerInfo.None
            | PacemakerMode.Score (rate, replay) ->
                let replayData = StoredReplayProvider(replay) :> IReplayProvider
                let scoring = createScoreMetric ruleset chart.Keys replayData chart.Notes rate
                PacemakerInfo.Replay scoring
            | PacemakerMode.Setting ->
                let setting = if options.Pacemakers.ContainsKey Rulesets.current_hash then options.Pacemakers.[Rulesets.current_hash] else Pacemaker.Default
                match setting with
                | Pacemaker.Accuracy acc -> PacemakerInfo.Accuracy acc
                | Pacemaker.Lamp lamp ->
                    let l = Rulesets.current.Grading.Lamps.[lamp]
                    PacemakerInfo.Judgement(l.Judgement, l.JudgementThreshold)

        let pacemakerMet(state: PlayState) =
            match state.Pacemaker with
            | PacemakerInfo.None -> true
            | PacemakerInfo.Accuracy x -> scoring.Value >= x
            | PacemakerInfo.Replay r -> r.Update Time.infinity; scoring.Value >= r.Value
            | PacemakerInfo.Judgement (judgement, count) ->
                let actual = 
                    if judgement = -1 then scoring.State.ComboBreaks
                    else
                        let mutable c = scoring.State.Judgements.[judgement]
                        for j = judgement + 1 to scoring.State.Judgements.Length - 1 do
                            if scoring.State.Judgements.[j] > 0 then c <- 1000000
                        c
                actual <= count
        
        let binds = options.GameplayBinds.[chart.Keys - 3]
        let mutable inputKeyState = 0us

        scoring.OnHit.Add(fun h -> match h.Guts with Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1 | _ -> ())

        { new IPlayScreen(chart, pacemakerInfo, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x = add_widget (this, this.Playfield, this.State) x

                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter
                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget Pacemaker
                add_widget JudgementCounts
                add_widget JudgementMeter
                add_widget EarlyLateMeter

            override this.OnEnter(previous) =
                if previous <> Screen.Type.Play then Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1
                base.OnEnter(previous)

            override this.OnExit(next) =
                if next = Screen.Type.Score then Stats.session.PlaysCompleted <- Stats.session.PlaysCompleted + 1
                elif next = Screen.Type.Play then Stats.session.PlaysRetried <- Stats.session.PlaysRetried + 1
                else Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1
                if options.AutoCalibrateOffset.Value then AutomaticSync.apply(scoring)
                base.OnExit(next)

            override this.Update(elapsedTime, bounds) =
                Stats.session.PlayTime <- Stats.session.PlayTime + elapsedTime
                base.Update(elapsedTime, bounds)
                let now = Song.timeWithOffset()
                let chartTime = now - firstNote

                if not (liveplay :> IReplayProvider).Finished then
                    // feed keyboard input into the replay provider
                    Input.consumeGameplay(binds, fun column time isRelease ->
                        if time > now then Logging.Debug("Received input event from the future") else
                        if isRelease then inputKeyState <- Bitmask.unsetBit column inputKeyState
                        else inputKeyState <- Bitmask.setBit column inputKeyState
                        liveplay.Add(time, inputKeyState) )
                    this.State.Scoring.Update chartTime

                if (!|"retry").Tapped() then
                    Screen.changeNew (fun () -> play_screen(pacemakerMode) :> Screen.T) Screen.Type.Play Transitions.Flags.Default
                
                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then
                    liveplay.Finish()
                    Screen.changeNew
                        ( fun () ->
                            let sd =
                                ScoreInfoProvider (
                                    Gameplay.makeScore((liveplay :> IReplayProvider).GetFullReplay(), this.Chart.Keys),
                                    Gameplay.Chart.current.Value,
                                    this.State.Ruleset,
                                    ModChart = Gameplay.Chart.withMods.Value,
                                    Difficulty = Gameplay.Chart.rating.Value
                                )
                            (sd, Gameplay.setScore (pacemakerMet this.State) sd)
                            |> ScoreScreen
                        )
                        Screen.Type.Score
                        Transitions.Flags.Default
                
            override this.Draw() =
                base.Draw()
                if options.AutoCalibrateOffset.Value && this.State.CurrentChartTime() < 0.0f<ms> then
                    Text.drawB(Style.font, sprintf "Local offset: %.0fms" AutomaticSync.offset.Value, 20.0f, this.Bounds.Left + 20.0f, this.Bounds.Top + 20.0f, Colors.text_subheading)
        }

    let multiplayer_screen() =
        
        let chart = Gameplay.Chart.withMods.Value
        let ruleset = Rulesets.current
        let firstNote = chart.Notes.[0].Time
        let liveplay = LiveReplayProvider firstNote
        let scoring = createScoreMetric ruleset chart.Keys liveplay chart.Notes Gameplay.rate.Value
        
        let binds = options.GameplayBinds.[chart.Keys - 3]
        let mutable inputKeyState = 0us
        let mutable packet_count = 0

        Lobby.start_playing()
        Gameplay.Online.Multiplayer.add_own_replay (scoring, liveplay)

        scoring.OnHit.Add(fun h -> match h.Guts with Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1 | _ -> ())

        let send_replay_packet(now: Time) =
            if not (liveplay :> IReplayProvider).Finished then liveplay.Add(now, inputKeyState)
            use ms = new System.IO.MemoryStream()
            use bw = new System.IO.BinaryWriter(ms)
            liveplay.ExportLiveBlock bw
            Lobby.play_data(ms.ToArray())
            packet_count <- packet_count + 1

        { new IPlayScreen(chart, PacemakerInfo.None, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x = add_widget (this, this.Playfield, this.State) x

                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget ComboMeter
                add_widget ProgressMeter
                add_widget Pacemaker
                add_widget JudgementCounts
                add_widget JudgementMeter
                add_widget EarlyLateMeter
                add_widget MultiplayerScoreTracker

            override this.OnEnter(previous) =
                Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1
                base.OnEnter(previous)

            override this.OnExit(next) =
                if next = Screen.Type.Score then Stats.session.PlaysCompleted <- Stats.session.PlaysCompleted + 1
                else Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1
                if options.AutoCalibrateOffset.Value then  AutomaticSync.apply(scoring)
                if next <> Screen.Type.Score then Lobby.abandon_play()
                base.OnExit(next)

            override this.Update(elapsedTime, bounds) =
                Stats.session.PlayTime <- Stats.session.PlayTime + elapsedTime
                base.Update(elapsedTime, bounds)
                let now = Song.timeWithOffset()
                let chartTime = now - firstNote

                if not (liveplay :> IReplayProvider).Finished then

                    if chartTime / MULTIPLAYER_REPLAY_DELAY_MS / 1.0f<ms> |> floor |> int > packet_count then
                        send_replay_packet(now)

                    Input.consumeGameplay(binds, fun column time isRelease ->
                        if time > now then Logging.Debug("Received input event from the future") else
                        if isRelease then inputKeyState <- Bitmask.unsetBit column inputKeyState
                        else inputKeyState <- Bitmask.setBit column inputKeyState
                        liveplay.Add(time, inputKeyState) )
                    this.State.Scoring.Update chartTime
                
                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then
                    liveplay.Finish()
                    send_replay_packet(now)
                    Lobby.finish_playing()
                    Screen.changeNew
                        ( fun () ->
                            let sd =
                                ScoreInfoProvider (
                                    Gameplay.makeScore((liveplay :> IReplayProvider).GetFullReplay(), this.Chart.Keys),
                                    Gameplay.Chart.current.Value,
                                    this.State.Ruleset,
                                    ModChart = Gameplay.Chart.withMods.Value,
                                    Difficulty = Gameplay.Chart.rating.Value
                                )
                            (sd, Gameplay.setScore true sd)
                            |> ScoreScreen
                        )
                        Screen.Type.Score
                        Transitions.Flags.Default
        }