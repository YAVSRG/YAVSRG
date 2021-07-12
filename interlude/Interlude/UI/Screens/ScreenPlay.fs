namespace Interlude.UI

open OpenTK
open System
open System.Drawing
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

(*
    Handful of widgets that directly pertain to gameplay
    They can all be toggled/repositioned/configured using themes
*)

module GameplayWidgets = 
    type HitEvent = (struct(JudgementType * Time * Time))
    type Helper = {
        Scoring: ScoreMetric
        HP: IHealthBarSystem
        OnHit: IEvent<HitEvent>
    }
    
    type AccuracyMeter(conf: WidgetConfig.AccuracyMeter, helper) as this =
        inherit Widget()

        let color = new AnimationColorMixer(if conf.GradeColors then Themes.themeConfig.GradeColors.[0] else Color.White)
        let listener =
            if conf.GradeColors then
                helper.OnHit.Subscribe(fun _ -> color.SetColor(Themes.themeConfig.GradeColors.[Grade.calculate Themes.themeConfig.GradeThresholds helper.Scoring.State]))
            else null

        do
            this.Animation.Add(color)
            this.Add(new Components.TextBox(helper.Scoring.FormatAccuracy, (fun () -> color.GetColor()), 0.5f) |> Components.positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.7f))
            if conf.ShowName then
                this.Add(new Components.TextBox(Utils.K helper.Scoring.Name, (Utils.K Color.White), 0.5f) |> Components.positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 1.0f))
        
        override this.Dispose() =
            if isNull listener then () else listener.Dispose()

    type HitMeter(conf: WidgetConfig.HitMeter, helper) =
        inherit Widget()
        let hits = ResizeArray<struct (Time * float32 * int)>()
        let mutable w = 0.0f
        let listener =
            helper.OnHit.Subscribe(
                fun struct (judgement, delta, now) -> hits.Add(struct (now, delta/helper.Scoring.MissWindow * w * 0.5f, int judgement)))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if w = 0.0f then w <- Rect.width this.Bounds
            let now = Audio.timeWithOffset()
            while hits.Count > 0 && let struct (time, _, _) = (hits.[0]) in time + conf.AnimationTime * 1.0f<ms> < now do
                hits.RemoveAt(0)

        override this.Draw() =
            base.Draw()
            let struct (left, top, right, bottom) = this.Bounds
            let centre = (right + left) * 0.5f
            if conf.ShowGuide then
                Draw.rect
                    (Rect.create (centre - conf.Thickness) top (centre + conf.Thickness) bottom)
                    Color.White
                    Sprite.Default
            let now = Audio.timeWithOffset()
            for struct (time, pos, j) in hits do
                Draw.rect
                    (Rect.create (centre + pos - conf.Thickness) top (centre + pos + conf.Thickness) bottom)
                    (let c = Themes.themeConfig.JudgementColors.[j] in
                        Color.FromArgb(Math.Clamp(255 - int (255.0f * (now - time) / conf.AnimationTime), 0, 255), int c.R, int c.G, int c.B))
                    Sprite.Default

        override this.Dispose() =
            listener.Dispose()

    type JudgementMeter(conf: WidgetConfig.JudgementMeter, helper) =
        inherit Widget()
        let atime = conf.AnimationTime * 1.0f<ms>
        let mutable tier = 0
        let mutable late = 0
        let mutable time = -atime * 2.0f - Audio.LEADIN_TIME
        let texture = Themes.getTexture("judgements")
        let listener =
            helper.OnHit.Subscribe(
                fun struct (judge, delta, now) ->
                    if
                        match judge with
                        | JudgementType.RIDICULOUS
                        | JudgementType.MARVELLOUS -> conf.ShowRDMA
                        | JudgementType.OK
                        | JudgementType.NG -> conf.ShowOKNG
                        | _ -> true
                    then
                        let j = int judge in
                        if j >= tier || now - atime > time then
                            tier <- j
                            time <- now
                            late <- if delta > 0.0f<ms> then 1 else 0)
        override this.Draw() =
            let a = 255 - Math.Clamp(255.0f * (Audio.timeWithOffset() - time) / atime |> int, 0, 255)
            Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf (Color.FromArgb(a, Color.White))) (Sprite.gridUV(late, tier) texture)

        override this.Dispose() =
            listener.Dispose()

    type ComboMeter(conf: WidgetConfig.Combo, helper) as this =
        inherit Widget()
        let popAnimation = new AnimationFade(0.0f)
        let color = new AnimationColorMixer(Color.White)
        let mutable hits = 0
        let listener =
            helper.OnHit.Subscribe(
                fun _ ->
                    hits <- hits + 1
                    if (conf.LampColors && hits > 50) then
                        color.SetColor(Themes.themeConfig.LampColors.[helper.Scoring.State |> Lamp.calculate |> int])
                    popAnimation.Value <- conf.Pop)

        do
            this.Animation.Add(color)
            this.Animation.Add(popAnimation)

        override this.Draw() =
            base.Draw()
            let combo = helper.Scoring.State.CurrentCombo
            let amt = popAnimation.Value + (((combo, 1000) |> Math.Min |> float32) * conf.Growth)
            Text.drawFill(Themes.font(), combo.ToString(), Rect.expand(amt, amt)this.Bounds, color.GetColor(), 0.5f)

        override this.Dispose() =
            listener.Dispose()

    type SkipButton(conf: WidgetConfig.SkipButton, helper) as this =
        inherit Widget()
        let firstNote = 
            let (_, notes, _, _, _) = Gameplay.getColoredChart()
            notes.First |> Option.map offsetOf |> Option.defaultValue 0.0f<ms>
        do
            this.Add(Components.TextBox(sprintf "Press %O to skip" options.Hotkeys.Skip.Value |> Utils.K, Utils.K Color.White, 0.5f))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if Audio.time() + Audio.LEADIN_TIME * 2.5f < firstNote then
                if options.Hotkeys.Skip.Value.Tapped() then
                    Audio.playFrom(firstNote - Audio.LEADIN_TIME)
            else this.Destroy()

    (*
        These widgets are not repositioned by theme
    *)

    type ColumnLighting(keys, binds: Bind array, lightTime, helper) as this =
        inherit Widget()
        let sliders = Array.init keys (fun _ -> new AnimationFade(0.0f))
        let sprite = Themes.getTexture("receptorlighting")
        let lightTime = Math.Min(0.99f, lightTime)

        do
            Array.iter this.Animation.Add sliders
            let hp = float32 Options.options.HitPosition.Value
            this.Reposition(0.0f, hp, 0.0f, -hp)

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            Array.iteri (fun k (s: AnimationFade) -> if helper.Scoring.KeyState |> Bitmap.hasBit k then s.Value <- 1.0f) sliders

        override this.Draw() =
            base.Draw()
            let struct (l, t, r, b) = this.Bounds
            let cw = (r - l) / (float32 keys)
            let threshold = 1.0f - lightTime
            let f k (s: AnimationFade) =
                if s.Value > threshold then
                    let p = (s.Value - threshold) / lightTime
                    let a = 255.0f * p |> int
                    Draw.rect
                        (
                            if Options.options.Upscroll.Value then
                                Sprite.alignedBoxX(l + cw * (float32 k + 0.5f), t, 0.5f, 1.0f, cw * p, -1.0f / p) sprite
                            else Sprite.alignedBoxX(l + cw * (float32 k + 0.5f), b, 0.5f, 1.0f, cw * p, 1.0f / p) sprite
                        )
                        (Color.FromArgb(a, Color.White))
                        sprite
            Array.iteri f sliders

open GameplayWidgets

type ScreenPlay() as this =
    inherit Screen()
    
    let chart = Gameplay.modifiedChart.Value
    let liveplay = new LiveReplayProvider()
    let scoring = createScoreMetric (fst options.AccSystems.Value) (fst options.HPSystems.Value) chart.Keys liveplay chart.Notes Gameplay.rate
    let onHit = new Event<HitEvent>()
    let widgetHelper: Helper = { Scoring = scoring; HP = scoring.HP; OnHit = onHit.Publish }
    let binds = Options.options.GameplayBinds.[chart.Keys - 3]
    let missWindow = scoring.ScaledMissWindow

    let mutable inputKeyState = 0us

    do
        let noteRenderer = new NoteRenderer()
        this.Add noteRenderer
        let inline f name (constructor: 'T -> Widget) = 
            let config: ^T = Themes.getGameplayConfig(name)
            let pos: WidgetConfig = (^T: (member Position: WidgetConfig) (config))
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
            noteRenderer.Add(new ColumnLighting(chart.Keys, binds, Themes.noteskinConfig.ColumnLightTime, widgetHelper))
        scoring.SetHitCallback(fun judge time -> onHit.Trigger(struct (judge, time, Audio.timeWithOffset())))

    override this.OnEnter(prev) =
        Screens.backgroundDim.Target <- float32 Options.options.BackgroundDim.Value
        //discord presence
        Screens.setToolbarCollapsed true
        Screens.setCursorVisible false
        Audio.changeRate Gameplay.rate
        Audio.trackFinishBehaviour <- Audio.TrackFinishBehaviour.Wait
        Audio.playLeadIn()
        //Screens.addDialog(new GameStartDialog())

    override this.OnExit next =
        Screens.backgroundDim.Target <- 0.7f
        Screens.setCursorVisible true
        if next = ScreenType.Score then () else
            Screens.setToolbarCollapsed false

    override this.Update(elapsedTime, bounds) =
        base.Update(elapsedTime, bounds)
        let now = Audio.timeWithOffset()
        if not liveplay.Finished then
            // feed keyboard input into the replay provider
            Input.consumeGameplay(binds, fun column time isRelease ->
                if isRelease then inputKeyState <- Bitmap.unsetBit column inputKeyState
                else inputKeyState <- Bitmap.setBit column inputKeyState
                liveplay.Add(time, inputKeyState) )
            scoring.Update(now)
        if now <= -missWindow && options.Hotkeys.Options.Value.Pressed() then Audio.pause(); Screens.addDialog(Screens.quickOptionsMenu())
        
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
