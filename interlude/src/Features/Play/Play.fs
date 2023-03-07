namespace Interlude.Features.Play

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Data.Scores
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Web.Shared
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play.GameplayWidgets
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
        let firstNote = offsetOf chart.Notes.First.Value
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

        { new IPlayScreen(chart, pacemakerInfo, ruleset, scoring) with
            override this.AddWidgets() =
                let inline add_widget x = add_widget (this, this.Playfield, this.State) x

                add_widget AccuracyMeter
                add_widget HitMeter
                add_widget LifeMeter
                add_widget ComboMeter
                add_widget SkipButton
                add_widget ProgressMeter
                add_widget Pacemaker
                add_widget JudgementCounts

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                let now = Song.timeWithOffset()
                let chartTime = now - firstNote

                if not (liveplay :> IReplayProvider).Finished then
                    // feed keyboard input into the replay provider
                    Input.consumeGameplay(binds, fun column time isRelease ->
                        if isRelease then inputKeyState <- Bitmap.unsetBit column inputKeyState
                        else inputKeyState <- Bitmap.setBit column inputKeyState
                        liveplay.Add(time, inputKeyState) )
                    this.State.Scoring.Update chartTime

                if (!|"options").Tapped() then
                    Song.pause()
                    inputKeyState <- 0us
                    liveplay.Add(now, inputKeyState)
                    QuickOptions.show(this.State.Scoring, fun () -> Screen.changeNew (fun () -> play_screen(pacemakerMode) :> Screen.T) Screen.Type.Play Transitions.Flags.Default)

                if (!|"retry").Pressed() then
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
        }

    let multiplayer_screen() =
        
        let chart = Gameplay.Chart.withMods.Value
        let ruleset = Rulesets.current
        let firstNote = offsetOf chart.Notes.First.Value
        let liveplay = LiveReplayProvider firstNote
        let scoring = createScoreMetric ruleset chart.Keys liveplay chart.Notes Gameplay.rate.Value
        
        let binds = options.GameplayBinds.[chart.Keys - 3]
        let mutable inputKeyState = 0us
        let mutable packet_count = 0

        Lobby.start_playing()
        Gameplay.Online.Multiplayer.add_own_replay scoring

        let send_replay_packet() =
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
                add_widget LifeMeter
                add_widget ComboMeter
                add_widget ProgressMeter
                add_widget Pacemaker
                add_widget JudgementCounts

            override this.OnExit(next) =
                if next <> Screen.Type.Score then Lobby.abandon_play()
                base.OnExit(next)

            override this.Update(elapsedTime, bounds) =
                base.Update(elapsedTime, bounds)
                let now = Song.timeWithOffset()
                let chartTime = now - firstNote


                if not (liveplay :> IReplayProvider).Finished then

                    if chartTime / MULTIPLAYER_REPLAY_DELAY_MS / 1.0f<ms> |> floor |> int > packet_count then
                        send_replay_packet()

                    Input.consumeGameplay(binds, fun column time isRelease ->
                        if isRelease then inputKeyState <- Bitmap.unsetBit column inputKeyState
                        else inputKeyState <- Bitmap.setBit column inputKeyState
                        liveplay.Add(time, inputKeyState) )
                    this.State.Scoring.Update chartTime
                
                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then
                    liveplay.Finish()
                    send_replay_packet()
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