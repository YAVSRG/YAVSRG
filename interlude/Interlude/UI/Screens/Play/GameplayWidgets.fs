namespace Interlude.UI.Screens.Play

open OpenTK
open System
open System.Drawing
open Prelude.Common
open Prelude.ChartFormats.Interlude
open Prelude.Scoring
open Prelude.Data.Themes
open Interlude
open Interlude.Graphics
open Interlude.Options
open Interlude.UI
open Interlude.UI.Components
open Interlude.UI.Animation

(*
    Handful of widgets that directly pertain to gameplay
    They can all be toggled/repositioned/configured using themes
*)

module GameplayWidgets = 

    type Helper = {
        Scoring: IScoreMetric
        HP: IHealthBarSystem
        OnHit: IEvent<HitEvent<HitEventGuts>>
        CurrentChartTime: unit -> ChartTime
    }
    
    type AccuracyMeter(conf: WidgetConfig.AccuracyMeter, helper) as this =
        inherit Widget()

        let color = new AnimationColorMixer(if conf.GradeColors then Content.themeConfig().GradeColors.[0] else Color.White)
        let listener =
            if conf.GradeColors then
                helper.OnHit.Subscribe(fun _ -> color.SetColor(Content.themeConfig().GradeColors.[Grade.calculate (Content.themeConfig().GradeThresholds) helper.Scoring.State]))
            else null

        do
            this.Animation.Add(color)
            this.Add(new TextBox(helper.Scoring.FormatAccuracy, (fun () -> color.GetColor()), 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.0f, 0.0f, 1.0f, 0.0f, 0.7f))
            if conf.ShowName then
                this.Add(new TextBox(Utils.K helper.Scoring.Name, (Utils.K Color.White), 0.5f) |> positionWidget(0.0f, 0.0f, 0.0f, 0.6f, 0.0f, 1.0f, 0.0f, 1.0f))
        
        override this.Dispose() =
            if isNull listener then () else listener.Dispose()

    type HitMeter(conf: WidgetConfig.HitMeter, helper) =
        inherit Widget()
        let hits = ResizeArray<struct (Time * float32 * int)>()
        let mutable w = 0.0f
        let listener =
            helper.OnHit.Subscribe(fun ev ->
                match ev.Guts with
                | Hit (judgement, delta, _) | Release (judgement, delta, _, _) ->
                    hits.Add (struct (ev.Time, delta / helper.Scoring.MissWindow * w * 0.5f, int judgement))
                | _ -> ())

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if w = 0.0f then w <- Rect.width this.Bounds
            let now = helper.CurrentChartTime()
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
            let now = helper.CurrentChartTime()
            for struct (time, pos, j) in hits do
                Draw.rect
                    (Rect.create (centre + pos - conf.Thickness) top (centre + pos + conf.Thickness) bottom)
                    (let c = Content.themeConfig().JudgementColors.[j] in
                        Color.FromArgb(Math.Clamp(255 - int (255.0f * (now - time) / conf.AnimationTime), 0, 255), int c.R, int c.G, int c.B))
                    Sprite.Default

        override this.Dispose() =
            listener.Dispose()

    type JudgementMeter(conf: WidgetConfig.JudgementMeter, helper) =
        inherit Widget()
        let atime = conf.AnimationTime * 1.0f<ms>
        let mutable tier = 0
        let mutable late = 0
        let mutable time = -Audio.LEADIN_TIME * 3.0f
        let texture = Content.getTexture "judgements"
        let listener =
            helper.OnHit.Subscribe(fun ev ->
                let (judge, delta) =
                    match ev.Guts with
                    | Hit (judge, delta, _)
                    | Release (judge, delta, _, _) -> (judge, delta)
                    | Hold -> (JudgementType.OK, 0.0f<ms>)
                if
                    match judge with
                    | JudgementType.RIDICULOUS
                    | JudgementType.MARVELLOUS -> conf.ShowRDMA
                    | JudgementType.OK
                    | JudgementType.NG -> conf.ShowOKNG
                    | _ -> true
                then
                    let j = int judge in
                    if j >= tier || ev.Time - atime > time then
                        tier <- j
                        time <- ev.Time
                        late <- if delta > 0.0f<ms> then 1 else 0 )
        override this.Draw() =
            let a = 255 - Math.Clamp(255.0f * (helper.CurrentChartTime() - time) / atime |> int, 0, 255)
            Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf (Color.FromArgb(a, Color.White))) (Sprite.gridUV (late, tier) texture)

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
                        color.SetColor(Content.themeConfig().LampColors.[helper.Scoring.State |> Lamp.calculate |> int])
                    popAnimation.Value <- conf.Pop)

        do
            this.Animation.Add(color)
            this.Animation.Add(popAnimation)

        override this.Draw() =
            base.Draw()
            let combo = helper.Scoring.State.CurrentCombo
            let amt = popAnimation.Value + (((combo, 1000) |> Math.Min |> float32) * conf.Growth)
            Text.drawFill(Content.font(), combo.ToString(), Rect.expand(amt, amt)this.Bounds, color.GetColor(), 0.5f)

        override this.Dispose() =
            listener.Dispose()

    type SkipButton(conf: WidgetConfig.SkipButton, helper) as this =
        inherit Widget()
        let firstNote = Gameplay.getColoredChart().Notes.First |> Option.map offsetOf |> Option.defaultValue 0.0f<ms>
        do this.Add(TextBox(sprintf "Press %O to skip" options.Hotkeys.Skip.Value |> Utils.K, Utils.K Color.White, 0.5f))

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if helper.CurrentChartTime() < -Audio.LEADIN_TIME * 2.5f then
                if options.Hotkeys.Skip.Value.Tapped() then
                    Audio.playFrom(firstNote - Audio.LEADIN_TIME)
            else this.Destroy()

    (*
        These widgets are configured by noteskin, not theme (and do not have positioning info)
    *)

    type ColumnLighting(keys, lightTime, helper) as this =
        inherit Widget()
        let sliders = Array.init keys (fun _ -> new AnimationFade(0.0f))
        let sprite = Content.getTexture "receptorlighting"
        let lightTime = Math.Min(0.99f, lightTime)

        do
            Array.iter this.Animation.Add sliders
            let hitpos = float32 Options.options.HitPosition.Value
            this.Reposition(0.0f, hitpos, 0.0f, -hitpos)

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            Array.iteri (fun k (s: AnimationFade) -> if helper.Scoring.KeyState |> Bitmap.hasBit k then s.Value <- 1.0f) sliders

        override this.Draw() =
            base.Draw()
            let struct (l, t, r, b) = this.Bounds
            let columnwidth = (r - l) / (float32 keys)
            let threshold = 1.0f - lightTime
            let f k (s: AnimationFade) =
                if s.Value > threshold then
                    let p = (s.Value - threshold) / lightTime
                    let a = 255.0f * p |> int
                    Draw.rect
                        (
                            if Options.options.Upscroll.Value then
                                Sprite.alignedBoxX(l + columnwidth * (float32 k + 0.5f), t, 0.5f, 1.0f, columnwidth * p, -1.0f / p) sprite
                            else Sprite.alignedBoxX(l + columnwidth * (float32 k + 0.5f), b, 0.5f, 1.0f, columnwidth * p, 1.0f / p) sprite
                        )
                        (Color.FromArgb(a, Color.White))
                        sprite
            Array.iteri f sliders

    type Explosions(keys, config: WidgetConfig.Explosions, helper) as this =
        inherit Widget()
        let sliders = Array.init keys (fun _ -> new AnimationFade(0.0f))
        let mem = Array.zeroCreate keys
        let holding = Array.create keys false
        let explodeTime = Math.Min(0.99f, config.FadeTime)
        let animation = new AnimationCounter(config.AnimationFrameTime)

        let handleEvent (ev: HitEvent<HitEventGuts>) =
            match ev.Guts with
            | Hit (judge, _, true) when (config.ExplodeOnMiss || judge <> JudgementType.MISS) ->
                sliders.[ev.Column].Target <- 1.0f
                sliders.[ev.Column].Value <- 1.0f
                holding.[ev.Column] <- true
                mem.[ev.Column] <- ev.Guts
            | Hold ->
                sliders.[ev.Column].Target <- 1.0f
                sliders.[ev.Column].Value <- 1.0f
                holding.[ev.Column] <- true
                mem.[ev.Column] <- ev.Guts
            | Hit (judge, _, false) when (config.ExplodeOnMiss || judge <> JudgementType.MISS) ->
                sliders.[ev.Column].Value <- 1.0f
                mem.[ev.Column] <- ev.Guts
            | _ -> ()

        do
            this.Animation.Add animation
            Array.iter this.Animation.Add sliders
            let hitpos = float32 Options.options.HitPosition.Value
            this.Reposition(0.0f, hitpos, 0.0f, -hitpos)
            helper.OnHit.Add handleEvent

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            for k = 0 to (keys - 1) do
                if holding.[k] && helper.Scoring.KeyState |> Bitmap.hasBit k |> not then
                    holding.[k] <- false
                    sliders.[k].Target <- 0.0f

        override this.Draw() =
            base.Draw()
            let struct (l, t, r, b) = this.Bounds
            let columnwidth = (r - l) / (float32 keys)
            let threshold = 1.0f - explodeTime
            let f k (s: AnimationFade) =
                if s.Value > threshold then
                    let p = (s.Value - threshold) / explodeTime
                    let a = 255.0f * p |> int
                    
                    let box =
                        if Options.options.Upscroll.Value then Rect.createWH (l + columnwidth * float32 k) t columnwidth columnwidth
                        else Rect.createWH (l + columnwidth * float32 k) (b - columnwidth) columnwidth columnwidth
                        |> Rect.expand(config.ExpandAmount * (1.0f - p) * columnwidth, config.ExpandAmount * (1.0f - p) * columnwidth)
                    match mem.[k] with
                    | Hold ->
                        Draw.quad
                            (box |> Quad.ofRect |> Quad.rotateDeg (NoteRenderer.noteRotation keys k))
                            (Quad.colorOf (Color.FromArgb(a, Color.White)))
                            (Sprite.gridUV (animation.Loops, 0) (Content.getTexture "holdexplosion"))
                    | Hit (judge, _, true) ->
                        Draw.quad
                            (box |> Quad.ofRect |> Quad.rotateDeg (NoteRenderer.noteRotation keys k))
                            (Quad.colorOf (Color.FromArgb(a, Color.White)))
                            (Sprite.gridUV (animation.Loops, int judge) (Content.getTexture "holdexplosion"))
                    | Hit (judge, _, false) ->
                        Draw.quad
                            (box |> Quad.ofRect |> Quad.rotateDeg (NoteRenderer.noteRotation keys k))
                            (Quad.colorOf (Color.FromArgb(a, Color.White)))
                            (Sprite.gridUV (animation.Loops, int judge) (Content.getTexture "noteexplosion"))
                    | _ -> ()
            Array.iteri f sliders
