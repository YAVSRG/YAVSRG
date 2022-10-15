namespace Interlude.Features.Play

open System
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude.Common
open Prelude.Charts.Formats.Interlude
open Prelude.Scoring
open Prelude.Scoring.Grading
open Prelude.Data.Themes
open Interlude
open Interlude.Options
open Interlude.UI
open Interlude.Features

(*
    Handful of widgets that directly pertain to gameplay
    They can all be toggled/repositioned/configured using themes
*)

module GameplayWidgets = 

    type Helper = {
        ScoringConfig: Ruleset
        Scoring: IScoreMetric
        HP: HealthBarMetric
        OnHit: IEvent<HitEvent<HitEventGuts>>
        CurrentChartTime: unit -> ChartTime
    }
    
    type AccuracyMeter(conf: WidgetConfig.AccuracyMeter, helper) as this =
        inherit StaticContainer(NodeType.None)

        let grades = helper.ScoringConfig.Grading.Grades
        let color = Animation.Color (if conf.GradeColors then Array.last(grades).Color else Color.White)

        do
            if conf.GradeColors then
                helper.OnHit.Add
                    ( fun _ ->
                        Grade.calculate grades helper.Scoring.State |> helper.ScoringConfig.GradeColor |> color.SetColor
                    )

            this
            |* Text(
                helper.Scoring.FormatAccuracy,
                Color = (fun () -> color.GetColor(), Color.Transparent),
                Align = Alignment.CENTER,
                Position = { Position.Default with Bottom = 0.7f %+ 0.0f })
            if conf.ShowName then
                this
                |* Text(helper.Scoring.Name,
                    Color = Utils.K (Color.White, Color.Transparent),
                    Align = Alignment.CENTER,
                    Position = { Position.Default with Top = 0.6f %+ 0.0f })

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            color.Update elapsedTime

    [<Struct>]
    type private HitMeterHit = { Time: Time; Position: float32; IsRelease: bool; Judgement: JudgementId option }

    type HitMeter(conf: WidgetConfig.HitMeter, helper) =
        inherit StaticWidget(NodeType.None)
        let hits = ResizeArray<HitMeterHit>()
        let mutable w = 0.0f

        do
            helper.OnHit.Add(fun ev ->
                match ev.Guts with
                | Hit e ->
                    hits.Add { Time = ev.Time; Position = e.Delta / helper.Scoring.MissWindow * w * 0.5f; IsRelease = false; Judgement = e.Judgement }
                | Release e ->
                    hits.Add { Time = ev.Time; Position = e.Delta / helper.Scoring.MissWindow * w * 0.5f; IsRelease = true; Judgement = e.Judgement }
            )

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            if w = 0.0f then w <- this.Bounds.Width
            let now = helper.CurrentChartTime()
            while hits.Count > 0 && hits.[0].Time + conf.AnimationTime * 1.0f<ms> < now do
                hits.RemoveAt(0)

        override this.Draw() =
            let centre = this.Bounds.CenterX
            if conf.ShowGuide then
                Draw.rect
                    (Rect.Create(centre - conf.Thickness, this.Bounds.Top, centre + conf.Thickness, this.Bounds.Bottom))
                    Color.White
            let now = helper.CurrentChartTime()
            for hit in hits do
                let r = Rect.Create(centre + hit.Position - conf.Thickness, this.Bounds.Top, centre + hit.Position + conf.Thickness, this.Bounds.Bottom)
                let c = 
                    match hit.Judgement with
                    | None -> Color.FromArgb(Math.Clamp(127 - int (127.0f * (now - hit.Time) / conf.AnimationTime), 0, 127), Color.Silver)
                    | Some j -> Color.FromArgb(Math.Clamp(255 - int (255.0f * (now - hit.Time) / conf.AnimationTime), 0, 255), helper.ScoringConfig.JudgementColor j)
                Draw.rect (if hit.IsRelease then r.Expand(0.0f, conf.ReleasesExtraHeight) else r) c

    // disabled for now
    type JudgementMeter(conf: WidgetConfig.JudgementMeter, helper) =
        inherit StaticWidget(NodeType.None)
        let atime = conf.AnimationTime * 1.0f<ms>
        let mutable tier = 0
        let mutable late = 0
        let mutable time = -Time.infinity
        let texture = Content.getTexture "judgement"

        do
            helper.OnHit.Add
                ( fun ev ->
                    let (judge, delta) =
                        match ev.Guts with
                        | Hit e -> (e.Judgement, e.Delta)
                        | Release e -> (e.Judgement, e.Delta)
                    if
                        judge.IsSome && true
                        //match judge.Value with
                        //| _JType.RIDICULOUS
                        //| _JType.MARVELLOUS -> conf.ShowRDMA
                        //| _ -> true
                    then
                        let j = int judge.Value in
                        if j >= tier || ev.Time - atime > time then
                            tier <- j
                            time <- ev.Time
                            late <- if delta > 0.0f<ms> then 1 else 0
                )

        override this.Draw() =
            if time > -Time.infinity then
                let a = 255 - Math.Clamp(255.0f * (helper.CurrentChartTime() - time) / atime |> int, 0, 255)
                Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf (Color.FromArgb(a, Color.White))) (Sprite.gridUV (late, tier) texture)

    type ComboMeter(conf: WidgetConfig.Combo, helper) =
        inherit StaticWidget(NodeType.None)
        let popAnimation = Animation.Fade(0.0f)
        let color = Animation.Color(Color.White)
        let mutable hits = 0

        do
            helper.OnHit.Add(
                fun _ ->
                    hits <- hits + 1
                    if (conf.LampColors && hits > 50) then
                        Lamp.calculate helper.ScoringConfig.Grading.Lamps helper.Scoring.State
                        |> helper.ScoringConfig.LampColor
                        |> color.SetColor
                    popAnimation.Value <- conf.Pop)

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            color.Update elapsedTime
            popAnimation.Update elapsedTime

        override this.Draw() =
            let combo = helper.Scoring.State.CurrentCombo
            let amt = popAnimation.Value + (((combo, 1000) |> Math.Min |> float32) * conf.Growth)
            Text.drawFill(Style.baseFont, combo.ToString(), this.Bounds.Expand amt, color.GetColor(), 0.5f)

    type ProgressMeter(conf: WidgetConfig.ProgressMeter, helper) =
        inherit StaticWidget(NodeType.None)

        let duration = 
            let chart = Gameplay.Chart.colored()
            offsetOf chart.Notes.Last.Value - offsetOf chart.Notes.First.Value

        let pulse = Animation.Counter(1000.0)
        
        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            pulse.Update elapsedTime

        override this.Draw() =
            let height = this.Bounds.Height - conf.BarHeight
            let pc = helper.CurrentChartTime() / duration

            let bar = Rect.Box(this.Bounds.Left, (this.Bounds.Top + height * pc), this.Bounds.Width, conf.BarHeight)
            let glowA = (float conf.GlowColor.A) * pulse.Time / 1000.0 |> int
            Draw.rect (bar.Expand(conf.GlowSize)) (Color.FromArgb(glowA, conf.GlowColor))
            Draw.rect bar conf.BarColor

    type SkipButton(conf: WidgetConfig.SkipButton, helper) =
        inherit StaticWidget(NodeType.None)

        // todo: localise
        let text = sprintf "Press %O to skip" (!|"skip")
        let mutable active = true
        
        let firstNote = offsetOf (Gameplay.Chart.colored().Notes.First.Value)

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if active && helper.CurrentChartTime() < -Song.LEADIN_TIME * 2.5f then
                if (!|"skip").Tapped() then
                    Song.pause()
                    Song.playFrom(firstNote - Song.LEADIN_TIME)
            else active <- false

        override this.Draw() =
            if active then Text.drawFillB(Style.baseFont, text, this.Bounds, Style.text(), Alignment.CENTER)

    type LifeMeter(conf: WidgetConfig.LifeMeter, helper: Helper) =
        inherit StaticWidget(NodeType.None)

        let color = Animation.Color conf.FullColor
        let slider = Animation.Fade(float32 helper.HP.State.Health)

        override this.Update(elapsedTime, moved) =
            // todo: color nyi
            base.Update(elapsedTime, moved)
            slider.Target <- float32 helper.HP.State.Health
            color.Update elapsedTime
            slider.Update elapsedTime

        override this.Draw() =
            let w, h = this.Bounds.Width, this.Bounds.Height
            if conf.Horizontal then
                let b = this.Bounds.SliceLeft(w * float32 helper.HP.State.Health)
                Draw.rect b (color.GetColor 255)
                Draw.rect (b.SliceRight h) conf.EndColor
            else
                let b = this.Bounds.SliceBottom(h * float32 helper.HP.State.Health)
                Draw.rect b (color.GetColor 255)
                Draw.rect (b.SliceTop w) conf.EndColor

    (*
        These widgets are configured by noteskin, not theme (and do not have positioning info)
    *)

    type ColumnLighting(keys, lightTime, helper) as this =
        inherit StaticWidget(NodeType.None)
        let sliders = Array.init keys (fun _ -> Animation.Fade 0.0f)
        let sprite = Content.getTexture "receptorlighting"
        let lightTime = Math.Max(0.0f, Math.Min(0.99f, lightTime))

        do
            let hitpos = float32 options.HitPosition.Value
            this.Position <- { Position.Default with Top = 0.0f %+ hitpos; Bottom = 1.0f %- hitpos }

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            sliders |> Array.iter (fun s -> s.Update elapsedTime)
            Array.iteri (fun k (s: Animation.Fade) -> if helper.Scoring.KeyState |> Bitmap.hasBit k then s.Value <- 1.0f) sliders

        override this.Draw() =
            let columnwidth = this.Bounds.Width / (float32 keys)
            let threshold = 1.0f - lightTime
            let f k (s: Animation.Fade) =
                if s.Value > threshold then
                    let p = (s.Value - threshold) / lightTime
                    let a = 255.0f * p |> int
                    Draw.sprite
                        (
                            if options.Upscroll.Value then
                                Sprite.alignedBoxX(this.Bounds.Left + columnwidth * (float32 k + 0.5f), this.Bounds.Top, 0.5f, 1.0f, columnwidth * p, -1.0f / p) sprite
                            else Sprite.alignedBoxX(this.Bounds.Left + columnwidth * (float32 k + 0.5f), this.Bounds.Bottom, 0.5f, 1.0f, columnwidth * p, 1.0f / p) sprite
                        )
                        (Color.FromArgb(a, Color.White))
                        sprite
            Array.iteri f sliders

    type Explosions(keys, config: Prelude.Data.Themes.Explosions, helper) as this =
        inherit StaticWidget(NodeType.None)

        let sliders = Array.init keys (fun _ -> Animation.Fade 0.0f)
        let timers = Array.zeroCreate keys
        let mem = Array.zeroCreate keys
        let holding = Array.create keys false
        let explodeTime = Math.Min(0.99f, config.FadeTime)
        let animation = Animation.Counter config.AnimationFrameTime

        let handleEvent (ev: HitEvent<HitEventGuts>) =
            match ev.Guts with
            | Hit e when (config.ExplodeOnMiss || not e.Missed) ->
                sliders.[ev.Column].Target <- 1.0f
                sliders.[ev.Column].Value <- 1.0f
                timers.[ev.Column] <- ev.Time
                holding.[ev.Column] <- true
                mem.[ev.Column] <- ev.Guts
            | Hit e when (config.ExplodeOnMiss || not e.Missed) ->
                sliders.[ev.Column].Value <- 1.0f
                timers.[ev.Column] <- ev.Time
                mem.[ev.Column] <- ev.Guts
            | _ -> ()

        do
            let hitpos = float32 options.HitPosition.Value
            this.Position <- { Position.Default with Top = 0.0f %+ hitpos; Bottom = 1.0f %- hitpos }
            helper.OnHit.Add handleEvent

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            animation.Update elapsedTime
            sliders |> Array.iter (fun s -> s.Update elapsedTime)
            for k = 0 to (keys - 1) do
                if holding.[k] && helper.Scoring.KeyState |> Bitmap.hasBit k |> not then
                    holding.[k] <- false
                    sliders.[k].Target <- 0.0f

        override this.Draw() =
            let columnwidth = this.Bounds.Width / (float32 keys)
            let threshold = 1.0f - explodeTime
            let f k (s: Animation.Fade) =
                if s.Value > threshold then
                    let p = (s.Value - threshold) / explodeTime
                    let a = 255.0f * p |> int
                    
                    let box =
                        (
                            if options.Upscroll.Value then Rect.Box(this.Bounds.Left + columnwidth * float32 k, this.Bounds.Top, columnwidth, columnwidth)
                            else Rect.Box(this.Bounds.Left + columnwidth * float32 k, this.Bounds.Bottom - columnwidth, columnwidth, columnwidth)
                        )
                            .Expand((config.Scale - 1.0f) * columnwidth, (config.Scale - 1.0f) * columnwidth)
                            .Expand(config.ExpandAmount * (1.0f - p) * columnwidth, config.ExpandAmount * (1.0f - p) * columnwidth)
                    match mem.[k] with
                    | Hit e ->
                        let color = 
                            if config.Colors = ExplosionColors.Column then k
                            else match e.Judgement with Some j -> int j | None -> 0
                        let frame = (helper.CurrentChartTime() - timers.[k]) / toTime config.AnimationFrameTime |> int
                        Draw.quad
                            (box |> Quad.ofRect |> NoteRenderer.noteRotation keys k)
                            (Quad.colorOf (Color.FromArgb(a, Color.White)))
                            (Sprite.gridUV (frame, color) (Content.getTexture (if e.IsHold then "holdexplosion" else "noteexplosion")))
                    | _ -> ()
            Array.iteri f sliders

    // Screencover is controlled by game settings, not theme or noteskin

    type ScreenCover() =
        inherit StaticWidget(NodeType.None)

        override this.Draw() =
            
            if options.ScreenCover.Enabled.Value then

                let bounds = this.Bounds.Expand(0.0f, 2.0f)
                let fadeLength = float32 options.ScreenCover.FadeLength.Value
                let upper (amount: float32) =
                    Draw.rect (bounds.SliceTop(amount - fadeLength)) options.ScreenCover.Color.Value
                    Draw.quad
                        (bounds.SliceTop(amount).SliceBottom(fadeLength) |> Quad.ofRect)
                        struct (options.ScreenCover.Color.Value, options.ScreenCover.Color.Value, Color.FromArgb(0, options.ScreenCover.Color.Value), Color.FromArgb(0, options.ScreenCover.Color.Value))
                        Sprite.DefaultQuad
                let lower (amount: float32) =
                    Draw.rect (bounds.SliceBottom(amount - fadeLength)) options.ScreenCover.Color.Value
                    Draw.quad
                        (bounds.SliceBottom(amount).SliceTop(fadeLength) |> Quad.ofRect)
                        struct (Color.FromArgb(0, options.ScreenCover.Color.Value), Color.FromArgb(0, options.ScreenCover.Color.Value), options.ScreenCover.Color.Value, options.ScreenCover.Color.Value)
                        Sprite.DefaultQuad

                let height = bounds.Height

                let sudden = float32 options.ScreenCover.Sudden.Value * height
                let hidden = float32 options.ScreenCover.Hidden.Value * height

                if options.Upscroll.Value then upper hidden; lower sudden
                else lower hidden; upper sudden