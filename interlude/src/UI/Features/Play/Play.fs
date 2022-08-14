namespace Interlude.UI.Features.Play

open OpenTK
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Data.Themes
open Prelude.Data.Scores
open Interlude
open Interlude.Options
open Interlude.UI
open Interlude.UI.Features.Play.GameplayWidgets

type PlayScreen() as this =
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
            CurrentChartTime = fun () -> Song.timeWithOffset() - firstNote
        }
    let binds = options.GameplayBinds.[chart.Keys - 3]

    let mutable inputKeyState = 0us

    do
        let noteRenderer = NoteRenderer scoring
        this.Add noteRenderer

        if Content.noteskinConfig().ColumnLightTime >= 0.0f then
            noteRenderer.Add(new ColumnLighting(chart.Keys, Content.noteskinConfig().ColumnLightTime, widgetHelper))

        if Content.noteskinConfig().Explosions.FadeTime >= 0.0f then
            noteRenderer.Add(new Explosions(chart.Keys, Content.noteskinConfig().Explosions, widgetHelper))

        noteRenderer.Add(ScreenCover())

        let inline f name (constructor: 'T -> Widget1) = 
            let config: ^T = Content.getGameplayConfig<'T>()
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
            if pos.Enabled then
                (constructor config).Position { Left = pos.LeftA %+ pos.Left; Top = pos.TopA %+ pos.Top; Right = pos.RightA %+ pos.Right; Bottom = pos.BottomA %+ pos.Bottom }
                |> if pos.Float then this.Add else noteRenderer.Add

        f "accuracyMeter" (fun c -> new AccuracyMeter(c, widgetHelper) :> Widget1)
        f "hitMeter" (fun c -> new HitMeter(c, widgetHelper) :> Widget1)
        f "lifeMeter" (fun c -> new LifeMeter(c, widgetHelper) :> Widget1)
        f "combo" (fun c -> new ComboMeter(c, widgetHelper) :> Widget1)
        //f "judgementMeter" (fun c -> new JudgementMeter(c, widgetHelper) :> Widget)
        f "skipButton" (fun c -> new SkipButton(c, widgetHelper) :> Widget1)
        f "progressMeter" (fun c -> new ProgressMeter(c, widgetHelper) :> Widget1)

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

        if (!|"options").Pressed() then
            Song.pause()
            inputKeyState <- 0us
            liveplay.Add(now, inputKeyState)
            QuickOptions.show(scoring, fun () -> Screen.changeNew (fun () -> PlayScreen() :> Screen.T) Screen.Type.Play Transitions.Flags.Default)

        if (!|"retry").Pressed() then
            Screen.changeNew (fun () -> PlayScreen() :> Screen.T) Screen.Type.Play Transitions.Flags.Default
        
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
                    |> Features.Score.ScoreScreen
                    :> Screen.T
                )
                Screen.Type.Score
                Transitions.Flags.Default

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
            CurrentChartTime = fun () -> Song.timeWithOffset() - firstNote
        }

    do
        let noteRenderer = NoteRenderer scoring
        this.Add noteRenderer

        if Content.noteskinConfig().ColumnLightTime >= 0.0f then
            noteRenderer.Add(new ColumnLighting(chart.Keys, Content.noteskinConfig().ColumnLightTime, widgetHelper))

        if Content.noteskinConfig().Explosions.FadeTime >= 0.0f then
            noteRenderer.Add(new Explosions(chart.Keys, Content.noteskinConfig().Explosions, widgetHelper))

        noteRenderer.Add(ScreenCover())

        let inline f name (constructor: 'T -> Widget1) = 
            let config: ^T = Content.getGameplayConfig<'T>()
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
            if pos.Enabled then
                (constructor config)
                    .Position { Left = pos.LeftA %+ pos.Left; Top = pos.TopA %+ pos.Top; Right = pos.RightA %+ pos.Right; Bottom = pos.BottomA %+ pos.Bottom }
                |> if pos.Float then this.Add else noteRenderer.Add

        if not auto then
            f "accuracyMeter" (fun c -> new AccuracyMeter(c, widgetHelper) :> Widget1)
            f "hitMeter" (fun c -> new HitMeter(c, widgetHelper) :> Widget1)
            f "lifeMeter" (fun c -> new LifeMeter(c, widgetHelper) :> Widget1)
            f "combo" (fun c -> new ComboMeter(c, widgetHelper) :> Widget1)
            //f "judgementMeter" (fun c -> new JudgementMeter(c, widgetHelper) :> Widget)
        f "skipButton" (fun c -> new SkipButton(c, widgetHelper) :> Widget1)
        f "progressMeter" (fun c -> new ProgressMeter(c, widgetHelper) :> Widget1)

        scoring.SetHitCallback onHit.Trigger

    override this.OnEnter(prev) =
        Background.dim (float32 options.BackgroundDim.Value)
        Screen.Toolbar.hide()
        Gameplay.rate.Value <- rate
        Song.changeRate rate
        Song.changeGlobalOffset (toTime options.AudioOffset.Value)
        Song.onFinish <- SongFinishAction.Wait
        Song.playLeadIn()
        Input.finish_frame_events()

    override this.OnExit next =
        Background.dim 0.7f
        Screen.Toolbar.show()

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Song.timeWithOffset()
        let chartTime = now - firstNote

        if not keypressData.Finished then scoring.Update chartTime

        if (!|"options").Pressed() then
            QuickOptions.show(scoring, ignore)
        
        if keypressData.Finished then Screen.back Transitions.Flags.Default