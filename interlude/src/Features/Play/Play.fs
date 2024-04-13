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
open Interlude.Web.Shared
open Interlude.Features
open Interlude.Features.Gameplay.Chart
open Interlude.Features.Stats
open Interlude.Features.Online
open Interlude.Features.Score
open Interlude.Features.Play.HUD

[<RequireQualifiedAccess>]
type PacemakerMode =
    | None
    | Score of rate: float32 * ReplayData
    | Setting

module PlayScreen =

    let rec play_screen (info: LoadedChartInfo, pacemaker_mode: PacemakerMode) =

        let ruleset = Rulesets.current
        let first_note = info.WithMods.FirstNote
        let liveplay = LiveReplayProvider first_note

        let scoring =
            Metrics.create ruleset info.WithMods.Keys liveplay info.WithMods.Notes Gameplay.rate.Value

        let pacemaker_info =
            match pacemaker_mode with
            | PacemakerMode.None -> PacemakerInfo.None
            | PacemakerMode.Score(rate, replay) ->
                let replay_data = StoredReplayProvider(replay) :> IReplayProvider

                let replay_scoring =
                    Metrics.create ruleset info.WithMods.Keys replay_data info.WithMods.Notes rate

                PacemakerInfo.Replay replay_scoring
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

        let binds = options.GameplayBinds.[info.WithMods.Keys - 3]
        let mutable key_state = 0us

        scoring.OnHit.Add(fun h ->
            match h.Guts with
            | Hit d when not d.Missed -> Stats.session.NotesHit <- Stats.session.NotesHit + 1
            | _ -> ()
        )

        let mutable recommended_offset = 0.0f

        let offset_setting = LocalAudioSync.offset_setting info.SaveData

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

            let close () =
                Screen.change_new
                    (fun () -> play_screen (info, pacemaker_mode) :> Screen.T)
                    Screen.Type.Play
                    Transitions.Flags.Default
                |> ignore

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
                        close ()
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
                        Song.pause ()
                        recommended_offset <- LocalAudioSync.get_automatic screen.State info.SaveData |> float32
                        offset_slider.Select false
                    ),
                OnClose = close,
                AutoCloseWhen = (fun (_: SlideoutContent) -> not offset_slider.Selected)
            )

        { new IPlayScreen(info.Chart, info.WithColors, pacemaker_info, scoring) with
            override this.AddWidgets() =
                let user_options = options.HUD.Value
                let noteskin_options = Content.NoteskinConfig.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, user_options, noteskin_options) position constructor

                if user_options.ComboEnabled then add_widget noteskin_options.ComboPosition Combo
                if user_options.SkipButtonEnabled then add_widget noteskin_options.SkipButtonPosition SkipButton
                if user_options.ProgressMeterEnabled then add_widget noteskin_options.ProgressMeterPosition ProgressMeter
                if user_options.AccuracyEnabled then add_widget noteskin_options.AccuracyPosition Accuracy
                if user_options.TimingDisplayEnabled then add_widget noteskin_options.TimingDisplayPosition TimingDisplay
                if this.State.Pacemaker <> PacemakerInfo.None then add_widget noteskin_options.PacemakerPosition Pacemaker
                if user_options.JudgementCounterEnabled then add_widget noteskin_options.JudgementCounterPosition JudgementCounter
                if user_options.JudgementMeterEnabled then add_widget noteskin_options.JudgementMeterPosition JudgementMeter
                if user_options.EarlyLateMeterEnabled then add_widget noteskin_options.EarlyLateMeterPosition EarlyLateMeter
                if user_options.RateModMeterEnabled then add_widget noteskin_options.RateModMeterPosition RateModMeter
                if user_options.BPMMeterEnabled then add_widget noteskin_options.BPMMeterPosition BPMMeter

                let offset_slideout = offset_slideout this

                let retry () =
                    Screen.change_new
                        (fun () -> play_screen (info, pacemaker_mode) :> Screen.T)
                        Screen.Type.Play
                        Transitions.Flags.Default
                    |> ignore

                let give_up () =
                    Screen.back Transitions.Flags.Default |> ignore

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
                |+ HotkeyAction("options", offset_slideout.Open)
                |* offset_slideout

            override this.OnEnter(previous) =
                if previous <> Screen.Type.Play then
                    Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1

                base.OnEnter(previous)

                DiscordRPC.playing_timed ("Playing", info.CacheInfo.Title, info.CacheInfo.Length / Gameplay.rate.Value)

            override this.OnExit(next) =
                if next = Screen.Type.Score then
                    Stats.session.PlaysCompleted <- Stats.session.PlaysCompleted + 1
                elif next = Screen.Type.Play then
                    Stats.session.PlaysRetried <- Stats.session.PlaysRetried + 1
                else
                    Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1

                if options.AutoCalibrateOffset.Value && recommended_offset = 0.0f then
                    LocalAudioSync.apply_automatic this.State info.SaveData

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

                if this.State.Scoring.Finished && not (liveplay :> IReplayProvider).Finished then
                    liveplay.Finish()

                    Screen.change_new
                        (fun () ->
                            let score_info =
                                Gameplay.score_info_from_gameplay
                                    info
                                    scoring
                                    ((liveplay :> IReplayProvider).GetFullReplay())

                            (score_info, Gameplay.set_score (pacemaker_met this.State) score_info info.SaveData, true)
                            |> ScoreScreen
                        )
                        Screen.Type.Score
                        Transitions.Flags.Default
                    |> ignore

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
                            else [(%%"options").ToString()] %> "play.localoffset.hint"
                        ),
                        20.0f,
                        this.Bounds.Left + 20.0f,
                        this.Bounds.Top + 20.0f,
                        Colors.text_subheading
                    )
        }

    open System.IO

    let multiplayer_screen (info: LoadedChartInfo, lobby: Lobby) =

        let ruleset = Rulesets.current
        let first_note = info.WithMods.FirstNote
        let liveplay = LiveReplayProvider first_note

        let scoring =
            Metrics.create ruleset info.WithMods.Keys liveplay info.WithMods.Notes Gameplay.rate.Value

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
                         Rate = Gameplay.rate.Value

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

        let send_replay_packet (now: Time) =
            if not (liveplay :> IReplayProvider).Finished then
                liveplay.Add(now, key_state)

            use ms = new MemoryStream()
            use bw = new BinaryWriter(ms)
            liveplay.ExportLiveBlock bw
            lobby.SendReplayData(ms.ToArray())
            packet_count <- packet_count + 1

        { new IPlayScreen(info.Chart, info.WithColors, PacemakerInfo.None, scoring) with
            override this.AddWidgets() =
                let user_options = options.HUD.Value
                let noteskin_options = Content.NoteskinConfig.HUD
                let inline add_widget position constructor =
                    add_widget (this, this.Playfield, this.State, user_options, noteskin_options) position constructor

                if user_options.ComboEnabled then add_widget noteskin_options.ComboPosition Combo
                if user_options.ProgressMeterEnabled then add_widget noteskin_options.ProgressMeterPosition ProgressMeter
                if user_options.AccuracyEnabled then add_widget noteskin_options.AccuracyPosition Accuracy
                if user_options.TimingDisplayEnabled then add_widget noteskin_options.TimingDisplayPosition TimingDisplay
                if this.State.Pacemaker <> PacemakerInfo.None then add_widget noteskin_options.PacemakerPosition Pacemaker
                if user_options.JudgementCounterEnabled then add_widget noteskin_options.JudgementCounterPosition JudgementCounter
                if user_options.JudgementMeterEnabled then add_widget noteskin_options.JudgementMeterPosition JudgementMeter
                if user_options.EarlyLateMeterEnabled then add_widget noteskin_options.EarlyLateMeterPosition EarlyLateMeter
                if user_options.RateModMeterEnabled then add_widget noteskin_options.RateModMeterPosition RateModMeter
                if user_options.BPMMeterEnabled then add_widget noteskin_options.BPMMeterPosition BPMMeter
                // todo: better positioning + ability to isolate and test this component
                add_widget noteskin_options.PacemakerPosition 
                    (fun (user_options, noteskin_options, state) -> MultiplayerScoreTracker(user_options, noteskin_options, state, lobby.Replays))

                let give_up () =
                    Screen.back Transitions.Flags.Default |> ignore

                this
                |* HotkeyHoldAction(
                    "exit",
                    (if options.HoldToGiveUp.Value then ignore else give_up),
                    (if options.HoldToGiveUp.Value then give_up else ignore)
                )

            override this.OnEnter(previous) =
                Stats.session.PlaysStarted <- Stats.session.PlaysStarted + 1
                base.OnEnter(previous)

            override this.OnExit(next) =
                if next = Screen.Type.Score then
                    Stats.session.PlaysCompleted <- Stats.session.PlaysCompleted + 1
                else
                    Stats.session.PlaysQuit <- Stats.session.PlaysQuit + 1

                if options.AutoCalibrateOffset.Value then
                    LocalAudioSync.apply_automatic this.State info.SaveData

                if next <> Screen.Type.Score then
                    lobby.AbandonPlaying()

                base.OnExit(next)

                DiscordRPC.playing_timed (
                    "Multiplayer",
                    info.CacheInfo.Title,
                    info.CacheInfo.Length / Gameplay.rate.Value
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
                    lobby.FinishPlaying()

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
                        Transitions.Flags.Default
                    |> ignore
        }
