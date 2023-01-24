namespace Interlude.Features.Play

open OpenTK
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Data.Themes
open Prelude.Data.Scores
open Interlude
open Interlude.Options
open Interlude.Content
open Interlude.UI
open Interlude.Features
open Interlude.Features.Play.GameplayWidgets
open Interlude.Features.Score

[<RequireQualifiedAccess>]
type PacemakerMode =
    | None
    | Score of rate: float32 * ReplayData
    | Setting

type PlayScreen(pacemakerMode: PacemakerMode) as this =
    inherit Screen()
    
    let chart = Gameplay.Chart.withMods.Value
    let firstNote = offsetOf chart.Notes.First.Value

    let liveplay = LiveReplayProvider firstNote
    let scoringConfig = Rulesets.current
    let scoring = createScoreMetric scoringConfig chart.Keys liveplay chart.Notes Gameplay.rate.Value
    let onHit = new Event<HitEvent<HitEventGuts>>()

    let pacemakerInfo =
        match pacemakerMode with
        | PacemakerMode.None -> PacemakerInfo.None
        | PacemakerMode.Score (rate, replay) ->
            let replayData = StoredReplayProvider(replay) :> IReplayProvider
            let scoring = createScoreMetric scoringConfig chart.Keys replayData chart.Notes rate
            PacemakerInfo.Replay scoring
        | PacemakerMode.Setting ->
            let setting = if options.Pacemakers.ContainsKey Rulesets.current_hash then options.Pacemakers.[Rulesets.current_hash] else Pacemaker.Default
            match setting with
            | Pacemaker.Accuracy acc -> PacemakerInfo.Accuracy acc
            | Pacemaker.Lamp lamp ->
                let l = Rulesets.current.Grading.Lamps.[lamp]
                PacemakerInfo.Judgement(l.Judgement, l.JudgementThreshold)

    let widgetHelper: Helper =
        {
            ScoringConfig = scoringConfig
            Scoring = scoring
            HP = scoring.HP
            OnHit = onHit.Publish
            CurrentChartTime = fun () -> Song.timeWithOffset() - firstNote
            Pacemaker = pacemakerInfo
        }

    let pacemakerMet() =
        match widgetHelper.Pacemaker with
        | PacemakerInfo.None -> true
        | PacemakerInfo.Accuracy x -> widgetHelper.Scoring.Value >= x
        | PacemakerInfo.Replay r -> r.Update Time.infinity; widgetHelper.Scoring.Value >= r.Value
        | PacemakerInfo.Judgement (judgement, count) ->
            let actual = 
                if judgement = -1 then widgetHelper.Scoring.State.ComboBreaks
                else
                    let mutable c = widgetHelper.Scoring.State.Judgements.[judgement]
                    for j = judgement + 1 to widgetHelper.Scoring.State.Judgements.Length - 1 do
                        if widgetHelper.Scoring.State.Judgements.[j] > 0 then c <- 1000000
                    c
            actual <= count
    
    let binds = options.GameplayBinds.[chart.Keys - 3]

    let mutable inputKeyState = 0us

    do
        let noteRenderer = NoteRenderer scoring
        this.Add noteRenderer

        if noteskinConfig().EnableColumnLight then
            noteRenderer.Add(new ColumnLighting(chart.Keys, noteskinConfig().ColumnLightTime, widgetHelper))

        if noteskinConfig().Explosions.FadeTime >= 0.0f then
            noteRenderer.Add(new Explosions(chart.Keys, noteskinConfig().Explosions, widgetHelper))

        noteRenderer.Add(LaneCover())

        let inline add_widget (constructor: 'T -> Widget) = 
            let config: ^T = getGameplayConfig<'T>()
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
            if pos.Enabled then
                let w = constructor config
                w.Position <- { Left = pos.LeftA %+ pos.Left; Top = pos.TopA %+ pos.Top; Right = pos.RightA %+ pos.Right; Bottom = pos.BottomA %+ pos.Bottom }
                if pos.Float then this.Add w else noteRenderer.Add w

        add_widget (fun c -> new AccuracyMeter(c, widgetHelper))
        add_widget (fun c -> new HitMeter(c, widgetHelper))
        add_widget (fun c -> new LifeMeter(c, widgetHelper))
        add_widget (fun c -> new ComboMeter(c, widgetHelper))
        add_widget (fun c -> new SkipButton(c, widgetHelper))
        add_widget (fun c -> new ProgressMeter(c, widgetHelper))
        add_widget (fun c -> new Pacemaker(c, widgetHelper))
        add_widget (fun c -> new JudgementCounts(c, widgetHelper))

        scoring.SetHitCallback onHit.Trigger

    override this.OnEnter(prev) =
        Background.dim (float32 options.BackgroundDim.Value)
        Screen.Toolbar.hide()
        Song.changeRate Gameplay.rate.Value
        Song.changeGlobalOffset (toTime options.AudioOffset.Value)
        Song.onFinish <- SongFinishAction.Wait
        Song.playLeadIn()
        Input.finish_frame_events()

    override this.OnExit next =
        Background.dim 0.7f
        if next <> Screen.Type.Score then Screen.Toolbar.show()

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
            scoring.Update chartTime

        if (!|"options").Tapped() then
            Song.pause()
            inputKeyState <- 0us
            liveplay.Add(now, inputKeyState)
            QuickOptions.show(scoring, fun () -> Screen.changeNew (fun () -> PlayScreen(pacemakerMode) :> Screen.T) Screen.Type.Play Transitions.Flags.Default)

        if (!|"retry").Pressed() then
            Screen.changeNew (fun () -> PlayScreen(pacemakerMode) :> Screen.T) Screen.Type.Play Transitions.Flags.Default
        
        if scoring.Finished && not (liveplay :> IReplayProvider).Finished then
            liveplay.Finish()
            Screen.changeNew
                ( fun () ->
                    let sd =
                        ScoreInfoProvider (
                            Gameplay.makeScore((liveplay :> IReplayProvider).GetFullReplay(), chart.Keys),
                            Gameplay.Chart.current.Value,
                            scoringConfig,
                            ModChart = Gameplay.Chart.withMods.Value,
                            Difficulty = Gameplay.Chart.rating.Value
                        )
                    (sd, Gameplay.setScore (pacemakerMet()) sd)
                    |> ScoreScreen
                )
                Screen.Type.Score
                Transitions.Flags.Default