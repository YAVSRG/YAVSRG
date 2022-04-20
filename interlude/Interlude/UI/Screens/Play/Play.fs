namespace Interlude.UI.Screens.Play

open OpenTK
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Data.Themes
open Prelude.Data.Scores
open Interlude
open Interlude.Input
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Screens.Play.GameplayWidgets

type Screen() as this =
    inherit Screen.T()
    
    let chart = Gameplay.Chart.withMods.Value
    let firstNote = offsetOf chart.Notes.First.Value

    let liveplay = LiveReplayProvider firstNote
    let scoringConfig = getCurrentRuleset()
    let scoring = createScoreMetric scoringConfig chart.Keys liveplay chart.Notes Gameplay.rate.Value
    let onHit = new Event<HitEvent<HitEventGuts>>()
    let widgetHelper: Helper =
        {
            ScoringConfig = scoringConfig
            Scoring = scoring
            HP = scoring.HP
            OnHit = onHit.Publish
            CurrentChartTime = fun () -> Audio.timeWithOffset() - firstNote
        }
    let binds = options.GameplayBinds.[chart.Keys - 3]
    let missWindow = scoring.ScaledMissWindow

    let mutable inputKeyState = 0us

    do
        let noteRenderer = NoteRenderer scoring
        this.Add noteRenderer

        if Content.noteskinConfig().ColumnLightTime >= 0.0f then
            noteRenderer.Add(new ColumnLighting(chart.Keys, Content.noteskinConfig().ColumnLightTime, widgetHelper))

        if Content.noteskinConfig().Explosions.FadeTime >= 0.0f then
            noteRenderer.Add(new Explosions(chart.Keys, Content.noteskinConfig().Explosions, widgetHelper))

        noteRenderer.Add(ScreenCover())

        let inline f name (constructor: 'T -> Widget) = 
            let config: ^T = Content.getGameplayConfig<'T>()
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
            if pos.Enabled then
                config
                |> constructor
                |> positionWidget(pos.Left, pos.LeftA, pos.Top, pos.TopA, pos.Right, pos.RightA, pos.Bottom, pos.BottomA)
                |> if pos.Float then this.Add else noteRenderer.Add

        f "accuracyMeter" (fun c -> new AccuracyMeter(c, widgetHelper) :> Widget)
        f "hitMeter" (fun c -> new HitMeter(c, widgetHelper) :> Widget)
        f "lifeMeter" (fun c -> new LifeMeter(c, widgetHelper) :> Widget)
        f "combo" (fun c -> new ComboMeter(c, widgetHelper) :> Widget)
        //f "judgementMeter" (fun c -> new JudgementMeter(c, widgetHelper) :> Widget)
        f "skipButton" (fun c -> new SkipButton(c, widgetHelper) :> Widget)
        f "progressMeter" (fun c -> new ProgressMeter(c, widgetHelper) :> Widget)

        scoring.SetHitCallback onHit.Trigger

    override this.OnEnter(prev) =
        Screen.backgroundDim.Target <- float32 options.BackgroundDim.Value
        Screen.hideToolbar <- true
        Audio.changeRate Gameplay.rate.Value
        Audio.changeGlobalOffset (toTime options.AudioOffset.Value)
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Wait
        Audio.playLeadIn()
        Input.absorbAll()

    override this.OnExit next =
        Screen.backgroundDim.Target <- 0.7f
        if next <> Screen.Type.Score then Screen.hideToolbar <- false

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Audio.timeWithOffset()
        let chartTime = now - firstNote

        if not (liveplay :> IReplayProvider).Finished then
            // feed keyboard input into the replay provider
            Input.consumeGameplay(binds, fun column time isRelease ->
                if isRelease then inputKeyState <- Bitmap.unsetBit column inputKeyState
                else inputKeyState <- Bitmap.setBit column inputKeyState
                liveplay.Add(time, inputKeyState) )
            scoring.Update chartTime

        if (!|Hotkey.Options).Pressed() then
            Audio.pause()
            inputKeyState <- 0us
            liveplay.Add(now, inputKeyState)
            QuickOptions.show(scoring, fun () -> Screen.changeNew (fun () -> Screen() :> Screen.T) Screen.Type.Play Screen.TransitionFlag.Default)
        
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
                    (sd, Gameplay.setScore sd)
                    |> Screens.Score.Screen
                    :> Screen.T
                )
                Screen.Type.Score
                Screen.TransitionFlag.Default

[<RequireQualifiedAccess>]
type ReplayMode =
    | Auto
    | Replay of rate: float32 * ReplayData

type ReplayScreen(mode: ReplayMode) as this =
    inherit Screen.T()
    
    let chart = Gameplay.Chart.withMods.Value
    let firstNote = offsetOf chart.Notes.First.Value

    let keypressData, auto, rate =
        match mode with
        | ReplayMode.Auto -> StoredReplayProvider.AutoPlay (chart.Keys, chart.Notes) :> IReplayProvider, true, Gameplay.rate.Value
        | ReplayMode.Replay (rate, data) -> StoredReplayProvider(data) :> IReplayProvider, false, rate

    let scoringConfig = getCurrentRuleset()
    let scoring = createScoreMetric scoringConfig chart.Keys keypressData chart.Notes rate
    let onHit = new Event<HitEvent<HitEventGuts>>()
    let widgetHelper: Helper =
        {
            ScoringConfig = scoringConfig
            Scoring = scoring
            HP = scoring.HP
            OnHit = onHit.Publish
            CurrentChartTime = fun () -> Audio.timeWithOffset() - firstNote
        }

    do
        let noteRenderer = NoteRenderer scoring
        this.Add noteRenderer

        if Content.noteskinConfig().ColumnLightTime >= 0.0f then
            noteRenderer.Add(new ColumnLighting(chart.Keys, Content.noteskinConfig().ColumnLightTime, widgetHelper))

        if Content.noteskinConfig().Explosions.FadeTime >= 0.0f then
            noteRenderer.Add(new Explosions(chart.Keys, Content.noteskinConfig().Explosions, widgetHelper))

        noteRenderer.Add(ScreenCover())

        let inline f name (constructor: 'T -> Widget) = 
            let config: ^T = Content.getGameplayConfig<'T>()
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
            if pos.Enabled then
                config
                |> constructor
                |> positionWidget(pos.Left, pos.LeftA, pos.Top, pos.TopA, pos.Right, pos.RightA, pos.Bottom, pos.BottomA)
                |> if pos.Float then this.Add else noteRenderer.Add

        if not auto then
            f "accuracyMeter" (fun c -> new AccuracyMeter(c, widgetHelper) :> Widget)
            f "hitMeter" (fun c -> new HitMeter(c, widgetHelper) :> Widget)
            f "lifeMeter" (fun c -> new LifeMeter(c, widgetHelper) :> Widget)
            f "combo" (fun c -> new ComboMeter(c, widgetHelper) :> Widget)
            //f "judgementMeter" (fun c -> new JudgementMeter(c, widgetHelper) :> Widget)
        f "skipButton" (fun c -> new SkipButton(c, widgetHelper) :> Widget)
        f "progressMeter" (fun c -> new ProgressMeter(c, widgetHelper) :> Widget)

        scoring.SetHitCallback onHit.Trigger

    override this.OnEnter(prev) =
        Screen.backgroundDim.Target <- float32 options.BackgroundDim.Value
        Screen.hideToolbar <- true
        Gameplay.rate.Value <- rate
        Audio.changeRate rate
        Audio.changeGlobalOffset (toTime options.AudioOffset.Value)
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Wait
        Audio.playLeadIn()
        Input.absorbAll()

    override this.OnExit next =
        Screen.backgroundDim.Target <- 0.7f
        Screen.hideToolbar <- false

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Audio.timeWithOffset()
        let chartTime = now - firstNote

        if not keypressData.Finished then scoring.Update chartTime

        if (!|Hotkey.Options).Pressed() then
            QuickOptions.show(scoring, ignore)
        
        if keypressData.Finished then Screen.back Screen.TransitionFlag.Default