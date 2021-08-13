namespace Interlude.UI

open OpenTK
open Prelude.Common
open Prelude.Charts.Interlude
open Prelude.Scoring
open Prelude.Scoring.Metrics
open Prelude.Data.Themes
open Prelude.Data.ScoreManager
open Interlude
open Interlude.Graphics
open Interlude.Input
open Interlude.Options
open Interlude.UI.Animation
open Interlude.UI.ScreenPlayComponents
open Interlude.UI.ScreenPlayComponents.GameplayWidgets

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
                AnimationAction(this.Close)
            )
        )

    override this.Draw() =
        let struct (left, top, right, bottom) = this.Bounds
        let w = right - left
        let bounds =
            let m = (top + bottom) * 0.5f
            Rect.create left (m - 100.0f) right (m + 100.0f)
        if anim1.Target = 1.0f then
            Draw.rect(bounds |> Rect.expand(0.0f, 10.0f) |> Rect.sliceRight(w * anim1.Value))(Screens.accentShade(255, 0.5f, 0.0f))(Sprite.Default)
            Draw.rect(bounds |> Rect.sliceLeft(w * anim1.Value))(Screens.accentShade(255, 1.0f, 0.0f))(Sprite.Default)
        else
            Draw.rect(bounds |> Rect.expand(0.0f, 10.0f) |> Rect.sliceLeft(w * anim1.Value))(Screens.accentShade(255, 0.5f, 0.0f))(Sprite.Default)
            Draw.rect(bounds |> Rect.sliceRight(w * anim1.Value))(Screens.accentShade(255, 1.0f, 0.0f))(Sprite.Default)

    override this.OnClose() = ()

type ScreenPlay() as this =
    inherit Screen()
    
    let chart = Gameplay.modifiedChart.Value
    let liveplay = new LiveReplayProvider()
    let scoring = createScoreMetric (fst options.AccSystems.Value) (fst options.HPSystems.Value) chart.Keys liveplay chart.Notes Gameplay.rate
    let onHit = new Event<HitEvent<HitEventGuts>>()
    let widgetHelper: Helper = { Scoring = scoring; HP = scoring.HP; OnHit = onHit.Publish }
    let binds = Options.options.GameplayBinds.[chart.Keys - 3]
    let missWindow = scoring.ScaledMissWindow

    let mutable inputKeyState = 0us
    let mutable playing = false

    do
        let noteRenderer = new NoteRenderer(scoring)
        this.Add noteRenderer
        let inline f name (constructor: 'T -> Widget) = 
            let config: ^T = Themes.getGameplayConfig(name)
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) config)
            if pos.Enabled then
                config
                |> constructor
                |> Components.positionWidget(pos.Left, pos.LeftA, pos.Top, pos.TopA, pos.Right, pos.RightA, pos.Bottom, pos.BottomA)
                |> if pos.Float then this.Add else noteRenderer.Add
        f "accuracyMeter" (fun c -> new AccuracyMeter(c, widgetHelper) :> Widget)
        f "hitMeter" (fun c -> new HitMeter(c, widgetHelper) :> Widget)
        f "combo" (fun c -> new ComboMeter(c, widgetHelper) :> Widget)
        f "skipButton" (fun c -> new SkipButton(c, widgetHelper) :> Widget)
        f "judgementMeter" (fun c -> new JudgementMeter(c, widgetHelper) :> Widget)
        //todo: rest of widgets

        if Themes.noteskinConfig.ColumnLightTime >= 0.0f then
            noteRenderer.Add(new ColumnLighting(chart.Keys, Themes.noteskinConfig.ColumnLightTime, widgetHelper))

        if Themes.noteskinConfig.Explosions.FadeTime >= 0.0f then
            noteRenderer.Add(new Explosions(chart.Keys, Themes.noteskinConfig.Explosions, widgetHelper))

        scoring.SetHitCallback onHit.Trigger

    override this.OnEnter(prev) =
        Screens.backgroundDim.Target <- float32 Options.options.BackgroundDim.Value
        //discord presence
        Screens.setToolbarCollapsed true
        Screens.setCursorVisible false
        Audio.changeRate Gameplay.rate
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Wait
        Audio.playLeadIn()
        //Screens.addDialog(new GameStartDialog())
        playing <- true

    override this.OnExit next =
        Screens.backgroundDim.Target <- 0.7f
        Screens.setCursorVisible true
        if next = ScreenType.Score then () else
            Screens.setToolbarCollapsed false

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Audio.timeWithOffset()
        if playing && not liveplay.Finished then
            // feed keyboard input into the replay provider
            Input.consumeGameplay(binds, fun column time isRelease ->
                if isRelease then inputKeyState <- Bitmap.unsetBit column inputKeyState
                else inputKeyState <- Bitmap.setBit column inputKeyState
                liveplay.Add(time, inputKeyState) )
            scoring.Update now
        if now <= -missWindow && options.Hotkeys.Options.Value.Pressed() then
            Audio.pause()
            inputKeyState <- 0us
            liveplay.Add(now, inputKeyState)
            Screens.addDialog(Screens.quickOptionsMenu())
        
        if scoring.Finished && not liveplay.Finished then
            liveplay.Finish()
            ((fun () ->
                let sd =
                    ScoreInfoProvider(
                        Gameplay.makeScore((liveplay :> IReplayProvider).GetFullReplay(), chart.Keys),
                        Gameplay.currentChart.Value,
                        fst options.AccSystems.Value,
                        fst options.HPSystems.Value,
                        ModChart = Gameplay.modifiedChart.Value,
                        Difficulty = Gameplay.difficultyRating.Value)
                (sd, Gameplay.setScore sd)
                |> ScreenScore
                :> Screen), ScreenType.Score, ScreenTransitionFlag.Default)
            |> Screens.newScreen
