namespace Interlude.UI.Screens.Play

open OpenTK
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Data.Themes
open Prelude.Data.Scores
open Interlude
open Interlude.Graphics
open Interlude.Input
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Animation
open Interlude.UI.Screens.Play.GameplayWidgets

(*
    WIP, will be a fancy animation when beginning to play a chart
*)

type GameStartDialog() as this =
    inherit Dialog()

    let anim1 = new AnimationFade(0.0f)

    do
        this.Animation.Add(anim1)
        anim1.Target <- 1.0f
        this.Animation.Add(
            Animation.Serial(
                AnimationTimer 600.0,
                AnimationAction(fun () -> anim1.Target <- 0.0f),
                AnimationAction(this.BeginClose)
            )
        )

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let w = right - left
        let bounds =
            let m = (top + bottom) * 0.5f
            Rect.create left (m - 100.0f) right (m + 100.0f)
        if anim1.Target = 1.0f then
            Draw.rect(bounds |> Rect.expand(0.0f, 10.0f) |> Rect.sliceRight(w * anim1.Value)) (Style.accentShade(255, 0.5f, 0.0f)) Sprite.Default
            Draw.rect(bounds |> Rect.sliceLeft(w * anim1.Value)) (Style.accentShade(255, 1.0f, 0.0f)) Sprite.Default
        else
            Draw.rect(bounds |> Rect.expand(0.0f, 10.0f) |> Rect.sliceLeft(w * anim1.Value)) (Style.accentShade(255, 0.5f, 0.0f)) Sprite.Default
            Draw.rect(bounds |> Rect.sliceRight(w * anim1.Value)) (Style.accentShade(255, 1.0f, 0.0f)) Sprite.Default

    override this.OnClose() = ()

type PlayScreenType =
    | Normal
    | Auto
    | Replay of ReplayData

type Screen(start: PlayScreenType) as this =
    inherit Screen.T()
    
    let chart = Gameplay.modifiedChart.Value
    let firstNote = offsetOf chart.Notes.First.Value

    let keypressData, watchingReplay, auto =
        match start with
        | Normal -> new LiveReplayProvider(firstNote) :> IReplayProvider, false, false
        | Auto -> StoredReplayProvider.AutoPlay (chart.Keys, chart.Notes) :> IReplayProvider, true, true
        | Replay data -> StoredReplayProvider(data) :> IReplayProvider, true, false
    let scoring = createScoreMetric (fst options.AccSystems.Value) chart.Keys keypressData chart.Notes Gameplay.rate.Value
    let onHit = new Event<HitEvent<HitEventGuts>>()
    let widgetHelper: Helper =
        { 
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
            let config: ^T = Content.GameplayConfig.get name
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
            f "judgementMeter" (fun c -> new JudgementMeter(c, widgetHelper) :> Widget)
        f "skipButton" (fun c -> new SkipButton(c, widgetHelper) :> Widget)
        f "progressMeter" (fun c -> new ProgressMeter(c, widgetHelper) :> Widget)

        scoring.SetHitCallback onHit.Trigger

    override this.OnEnter(prev) =
        Screen.backgroundDim.Target <- float32 options.BackgroundDim.Value
        //discord presence
        Screen.toolbar <- true
        Audio.changeRate Gameplay.rate.Value
        Audio.changeGlobalOffset (toTime options.AudioOffset.Value)
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Wait
        Audio.playLeadIn()
        //Screens.addDialog(new GameStartDialog())
        Input.absorbAll()

    override this.OnExit next =
        Screen.backgroundDim.Target <- 0.7f
        if next <> Screen.Type.Score then Screen.toolbar <- false

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Audio.timeWithOffset()
        let chartTime = now - firstNote

        if not keypressData.Finished then
            if not watchingReplay then
                let liveplay = keypressData :?> LiveReplayProvider
                // feed keyboard input into the replay provider
                Input.consumeGameplay(binds, fun column time isRelease ->
                    if isRelease then inputKeyState <- Bitmap.unsetBit column inputKeyState
                    else inputKeyState <- Bitmap.setBit column inputKeyState
                    liveplay.Add(time, inputKeyState) )
            scoring.Update chartTime

        if now <= -missWindow && options.Hotkeys.Options.Value.Pressed() then
            Audio.pause()
            inputKeyState <- 0us
            if not watchingReplay then (keypressData :?> LiveReplayProvider).Add(now, inputKeyState)
            QuickOptions.show()
        
        if watchingReplay && keypressData.Finished then Screen.back Screen.TransitionFlag.Default
        elif scoring.Finished && not keypressData.Finished then
            (keypressData :?> LiveReplayProvider).Finish()
            Screen.changeNew
                ( fun () ->
                    let sd =
                        ScoreInfoProvider (
                            Gameplay.makeScore(keypressData.GetFullReplay(), chart.Keys),
                            Gameplay.currentChart.Value,
                            fst options.AccSystems.Value,
                            Content.themeConfig().Grades,
                            ModChart = Gameplay.modifiedChart.Value,
                            Difficulty = Gameplay.difficultyRating.Value
                        )
                    (sd, if not watchingReplay then Gameplay.setScore sd else BestFlags.Default)
                    |> Screens.Score.Screen
                    :> Screen.T
                )
                Screen.Type.Score
                Screen.TransitionFlag.Default
