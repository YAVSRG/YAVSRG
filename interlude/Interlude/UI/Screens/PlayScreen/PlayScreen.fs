namespace Interlude.UI

open OpenTK
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
open Interlude.UI.Gameplay
open Interlude.UI.Gameplay.GameplayWidgets

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
            Draw.rect(bounds |> Rect.expand(0.0f, 10.0f) |> Rect.sliceRight(w * anim1.Value))(ScreenGlobals.accentShade(255, 0.5f, 0.0f))(Sprite.Default)
            Draw.rect(bounds |> Rect.sliceLeft(w * anim1.Value))(ScreenGlobals.accentShade(255, 1.0f, 0.0f))(Sprite.Default)
        else
            Draw.rect(bounds |> Rect.expand(0.0f, 10.0f) |> Rect.sliceLeft(w * anim1.Value))(ScreenGlobals.accentShade(255, 0.5f, 0.0f))(Sprite.Default)
            Draw.rect(bounds |> Rect.sliceRight(w * anim1.Value))(ScreenGlobals.accentShade(255, 1.0f, 0.0f))(Sprite.Default)

    override this.OnClose() = ()

type PlayScreenType =
    | Normal
    | Auto
    | Replay of ReplayData

type PlayScreen(start: PlayScreenType) as this =
    inherit Screen()
    
    let chart = Gameplay.modifiedChart.Value
    let liveplay, allowInput =
        match start with
        | Normal -> new LiveReplayProvider() :> IReplayProvider, true
        | Auto -> StoredReplayProvider.AutoPlay (chart.Keys, chart.Notes) :> IReplayProvider, false
        | Replay data -> StoredReplayProvider(data) :> IReplayProvider, false
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
        ScreenGlobals.backgroundDim.Target <- float32 Options.options.BackgroundDim.Value
        //discord presence
        ScreenGlobals.setToolbarCollapsed true
        ScreenGlobals.setCursorVisible false
        Audio.changeRate Gameplay.rate
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Wait
        Audio.playLeadIn()
        //Screens.addDialog(new GameStartDialog())
        playing <- true
        Input.absorbAll()

    override this.OnExit next =
        ScreenGlobals.backgroundDim.Target <- 0.7f
        ScreenGlobals.setCursorVisible true
        if next = ScreenType.Score then () else
            ScreenGlobals.setToolbarCollapsed false

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Audio.timeWithOffset()

        if allowInput then
            let liveplay = liveplay :?> LiveReplayProvider
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
            if allowInput then (liveplay :?> LiveReplayProvider).Add(now, inputKeyState)
            ScreenGlobals.addDialog(ScreenGlobals.quickOptionsMenu())
        
        if scoring.Finished && (not allowInput || not (liveplay :?> LiveReplayProvider).Finished) then
            if allowInput then (liveplay :?> LiveReplayProvider).Finish()
            ((fun () ->
                let sd =
                    ScoreInfoProvider(
                        Gameplay.makeScore(liveplay.GetFullReplay(), chart.Keys),
                        Gameplay.currentChart.Value,
                        fst options.AccSystems.Value,
                        fst options.HPSystems.Value,
                        ModChart = Gameplay.modifiedChart.Value,
                        Difficulty = Gameplay.difficultyRating.Value)
                (sd, if allowInput then Gameplay.setScore sd else (PersonalBestType.None, PersonalBestType.None, PersonalBestType.None))
                |> ScoreScreen
                :> Screen), ScreenType.Score, ScreenTransitionFlag.Default)
            |> ScreenGlobals.newScreen
