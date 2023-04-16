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
open Interlude.Content
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Gameplay.Online
open Interlude.Utils

(*
    Handful of widgets that directly pertain to gameplay
    They can all be toggled/repositioned/configured using themes
*)

module GameplayWidgets =
    
    type AccuracyMeter(conf: WidgetConfig.AccuracyMeter, state) as this =
        inherit StaticContainer(NodeType.None)

        let grades = state.Ruleset.Grading.Grades
        let color = Animation.Color (if conf.GradeColors then Array.last(grades).Color else Color.White)

        do
            if conf.GradeColors then
                state.Scoring.OnHit.Add
                    ( fun _ ->
                        color.Target <- Grade.calculate grades state.Scoring.State |> state.Ruleset.GradeColor
                    )

            this
            |* Text(
                state.Scoring.FormatAccuracy,
                Color = (fun () -> color.Value, Color.Transparent),
                Align = Alignment.CENTER,
                Position = { Position.Default with Bottom = 0.7f %+ 0.0f })
            if conf.ShowName then
                this
                |* Text(state.Scoring.Name,
                    Color = K (Color.White, Color.Transparent),
                    Align = Alignment.CENTER,
                    Position = { Position.Default with Top = 0.6f %+ 0.0f })

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            color.Update elapsedTime

    [<Struct>]
    type private HitMeterHit = { Time: Time; Position: float32; IsRelease: bool; Judgement: JudgementId option }

    type HitMeter(conf: WidgetConfig.HitMeter, state) =
        inherit StaticWidget(NodeType.None)
        let hits = ResizeArray<HitMeterHit>()
        let mutable w = 0.0f

        do
            state.Scoring.OnHit.Add(fun ev ->
                match ev.Guts with
                | Hit e ->
                    hits.Add { Time = ev.Time; Position = e.Delta / state.Scoring.MissWindow * w * 0.5f; IsRelease = false; Judgement = e.Judgement }
                | Release e ->
                    hits.Add { Time = ev.Time; Position = e.Delta / state.Scoring.MissWindow * w * 0.5f; IsRelease = true; Judgement = e.Judgement }
            )

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            if w = 0.0f then w <- this.Bounds.Width
            let now = state.CurrentChartTime()
            while hits.Count > 0 && hits.[0].Time + conf.AnimationTime * 1.0f<ms> < now do
                hits.RemoveAt(0)

        override this.Draw() =
            let centre = this.Bounds.CenterX
            if conf.ShowGuide then
                Draw.rect
                    (Rect.Create(centre - conf.Thickness, this.Bounds.Top, centre + conf.Thickness, this.Bounds.Bottom))
                    Color.White
            let now = state.CurrentChartTime()
            for hit in hits do
                let r = Rect.Create(centre + hit.Position - conf.Thickness, this.Bounds.Top, centre + hit.Position + conf.Thickness, this.Bounds.Bottom)
                let c = 
                    match hit.Judgement with
                    | None -> Color.FromArgb(Math.Clamp(127 - int (127.0f * (now - hit.Time) / conf.AnimationTime), 0, 127), Color.Silver)
                    | Some j -> Color.FromArgb(Math.Clamp(255 - int (255.0f * (now - hit.Time) / conf.AnimationTime), 0, 255), state.Ruleset.JudgementColor j)
                if conf.ShowNonJudgements || hit.Judgement.IsSome then
                    Draw.rect (if hit.IsRelease then r.Expand(0.0f, conf.ReleasesExtraHeight) else r) c

    //type JudgementMeter(conf: WidgetConfig.JudgementMeter, helper) =
    //    inherit StaticWidget(NodeType.None)
    //    let atime = conf.AnimationTime * 1.0f<ms>
    //    let mutable tier = 0
    //    let mutable late = 0
    //    let mutable time = -Time.infinity
    //    let texture = Content.getTexture "judgement"

    //    do
    //        helper.OnHit.Add
    //            ( fun ev ->
    //                let (judge, delta) =
    //                    match ev.Guts with
    //                    | Hit e -> (e.Judgement, e.Delta)
    //                    | Release e -> (e.Judgement, e.Delta)
    //                if
    //                    judge.IsSome && true
    //                    //match judge.Value with
    //                    //| _JType.RIDICULOUS
    //                    //| _JType.MARVELLOUS -> conf.ShowRDMA
    //                    //| _ -> true
    //                then
    //                    let j = int judge.Value in
    //                    if j >= tier || ev.Time - atime > time then
    //                        tier <- j
    //                        time <- ev.Time
    //                        late <- if delta > 0.0f<ms> then 1 else 0
    //            )

    //    override this.Draw() =
    //        if time > -Time.infinity then
    //            let a = 255 - Math.Clamp(255.0f * (helper.CurrentChartTime() - time) / atime |> int, 0, 255)
    //            Draw.quad (Quad.ofRect this.Bounds) (Quad.colorOf (Color.FromArgb(a, Color.White))) (Sprite.gridUV (late, tier) texture)

    type ComboMeter(conf: WidgetConfig.Combo, state) =
        inherit StaticWidget(NodeType.None)
        let popAnimation = Animation.Fade(0.0f)
        let color = Animation.Color(Color.White)
        let mutable hits = 0

        do
            state.Scoring.OnHit.Add(
                fun _ ->
                    hits <- hits + 1
                    if (conf.LampColors && hits > 50) then
                        color.Target <-
                            Lamp.calculate state.Ruleset.Grading.Lamps state.Scoring.State
                            |> state.Ruleset.LampColor
                    popAnimation.Value <- conf.Pop)

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            color.Update elapsedTime
            popAnimation.Update elapsedTime

        override this.Draw() =
            let combo = state.Scoring.State.CurrentCombo
            let amt = popAnimation.Value + (((combo, 1000) |> Math.Min |> float32) * conf.Growth)
            Text.drawFill(Style.baseFont, combo.ToString(), this.Bounds.Expand amt, color.Value, 0.5f)

    type ProgressMeter(conf: WidgetConfig.ProgressMeter, state) =
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
            let pc = state.CurrentChartTime() / duration

            let bar = Rect.Box(this.Bounds.Left, (this.Bounds.Top + height * pc), this.Bounds.Width, conf.BarHeight)
            let glowA = (float conf.GlowColor.A) * pulse.Time / 1000.0 |> int
            Draw.rect (bar.Expand(conf.GlowSize)) (Color.FromArgb(glowA, conf.GlowColor))
            Draw.rect bar conf.BarColor

    type SkipButton(conf: WidgetConfig.SkipButton, state) =
        inherit StaticWidget(NodeType.None)

        let text = Localisation.localiseWith [(!|"skip").ToString()] "play.skiphint"
        let mutable active = true
        
        let firstNote = offsetOf (Gameplay.Chart.colored().Notes.First.Value)

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            if active && state.CurrentChartTime() < -Song.LEADIN_TIME * 2.5f then
                if (!|"skip").Tapped() then
                    Song.pause()
                    Song.playFrom(firstNote - Song.LEADIN_TIME)
            else active <- false

        override this.Draw() =
            if active then Text.drawFillB(Style.baseFont, text, this.Bounds, Style.text(), Alignment.CENTER)

    type LifeMeter(conf: WidgetConfig.LifeMeter, state: PlayState) =
        inherit StaticWidget(NodeType.None)

        let color = Animation.Color conf.FullColor
        let slider = Animation.Fade(float32 state.Scoring.HP.State.Health)

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            slider.Target <- float32 state.Scoring.HP.State.Health
            color.Target <- (Color.FromArgb(
                Percyqaz.Flux.Utils.lerp (float32 state.Scoring.HP.State.Health) (float32 conf.EmptyColor.R) (float32 conf.FullColor.R) |> int,
                Percyqaz.Flux.Utils.lerp (float32 state.Scoring.HP.State.Health) (float32 conf.EmptyColor.G) (float32 conf.FullColor.G) |> int,
                Percyqaz.Flux.Utils.lerp (float32 state.Scoring.HP.State.Health) (float32 conf.EmptyColor.B) (float32 conf.FullColor.B) |> int
                ))
            color.Update elapsedTime
            slider.Update elapsedTime

        override this.Draw() =
            let w, h = this.Bounds.Width, this.Bounds.Height
            if conf.Horizontal then
                let b = this.Bounds.SliceLeft(w * slider.Value)
                Draw.rect b color.Value
                Draw.rect (b.SliceRight h) conf.TipColor
            else
                let b = this.Bounds.SliceBottom(h * slider.Value)
                Draw.rect b color.Value
                Draw.rect (b.SliceTop w) conf.TipColor

    type Pacemaker(conf: WidgetConfig.Pacemaker, state: PlayState) =
        inherit StaticWidget(NodeType.None)

        let color = Animation.Color(Color.White)
        let flag_position = Animation.Fade(0.5f)
        let position_cooldown = Animation.Delay(3000.0)
        let mutable ahead_by = 0.0
        let mutable hearts = -1

        let update_flag_position() =
            if ahead_by >= 10.0 then flag_position.Target <- 1.0f
            elif ahead_by > -10.0 then 
                flag_position.Target <- (ahead_by + 10.0) / 20.0 |> float32
                if ahead_by > 0.0 then color.Target <- Color.FromHsv(140.0f/360.0f, ahead_by / 10.0 |> float32, 1.0f)
                else color.Target <- Color.FromHsv(340.0f/360.0f, ahead_by / -10.0 |> float32, 1.0f)
            else flag_position.Target <- 0.0f

        do
            match state.Pacemaker with
            | PacemakerInfo.None
            | PacemakerInfo.Accuracy _
            | PacemakerInfo.Replay _ -> ()
            | PacemakerInfo.Judgement (judgement, _) -> 
                color.Target <-
                    if judgement = -1 then 
                        Rulesets.current.Judgements.[Rulesets.current.Judgements.Length - 1].Color
                    else Rulesets.current.Judgements.[judgement].Color
            

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)

            match state.Pacemaker with
            | PacemakerInfo.None -> ()
            | PacemakerInfo.Accuracy x ->
                if position_cooldown.Complete then
                    ahead_by <- state.Scoring.State.PointsScored - state.Scoring.State.MaxPointsScored * x
                    update_flag_position()
                    position_cooldown.Reset()

                flag_position.Update elapsedTime
                position_cooldown.Update elapsedTime
            | PacemakerInfo.Replay score ->
                if position_cooldown.Complete then
                    score.Update(state.CurrentChartTime())
                    ahead_by <- state.Scoring.State.PointsScored - score.State.PointsScored
                    update_flag_position()
                    position_cooldown.Reset()

                flag_position.Update elapsedTime
                position_cooldown.Update elapsedTime
            | PacemakerInfo.Judgement (_, _) -> ()

            color.Update elapsedTime

        override this.Draw() =
            match state.Pacemaker with
            | PacemakerInfo.None -> ()
            | PacemakerInfo.Accuracy _
            | PacemakerInfo.Replay _ ->
                Text.drawFillB(Style.baseFont, Icons.goal, this.Bounds.SliceLeft(0.0f).Expand(this.Bounds.Height, 0.0f).Translate(this.Bounds.Width * flag_position.Value, 0.0f), (color.Value, Color.Black), Alignment.CENTER)
            | PacemakerInfo.Judgement (judgement, count) ->
                let actual = 
                    if judgement = -1 then state.Scoring.State.ComboBreaks
                    else
                        let mutable c = state.Scoring.State.Judgements.[judgement]
                        for j = judgement + 1 to state.Scoring.State.Judgements.Length - 1 do
                            if state.Scoring.State.Judgements.[j] > 0 then c <- 1000000
                        c
                let _hearts = 1 + count - actual
                if _hearts < hearts then color.Value <- Color.White
                hearts <- _hearts
                let display = 
                    if hearts > 5 then sprintf "%s x%i" (String.replicate 5 Icons.heart2) hearts
                    elif hearts > 0 then (String.replicate hearts Icons.heart2)
                    else Icons.failure
                Text.drawFillB(Style.baseFont, display, this.Bounds, (color.Value, Color.Black), Alignment.CENTER)

    type JudgementCounts(conf: WidgetConfig.JudgementCounts, state: PlayState) =
        inherit StaticWidget(NodeType.None)

        let judgementAnimations = Array.init state.Ruleset.Judgements.Length (fun _ -> Animation.Delay(conf.AnimationTime))

        do
            state.Scoring.OnHit.Add (
                fun h -> 
                    match h.Guts with 
                    | Hit x -> if x.Judgement.IsSome then judgementAnimations[x.Judgement.Value].Reset()
                    | Release x -> if x.Judgement.IsSome then judgementAnimations[x.Judgement.Value].Reset()
                )

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            for j in judgementAnimations do
                j.Update elapsedTime

        override this.Draw() =
            let h = this.Bounds.Height / float32 judgementAnimations.Length
            let mutable r = this.Bounds.SliceTop(h).Shrink(5.0f)
            for i = 0 to state.Ruleset.Judgements.Length - 1 do
                let j = state.Ruleset.Judgements.[i]
                Draw.rect (r.Expand(10.0f, 5.0f).SliceLeft(5.0f)) j.Color
                if not judgementAnimations.[i].Complete && state.Scoring.State.Judgements.[i] > 0 then
                    Draw.rect (r.Expand 5.0f) (Color.FromArgb(127 - max 0 (int (127.0 * judgementAnimations.[i].Elapsed / conf.AnimationTime)), j.Color))
                Text.drawFillB(Style.baseFont, j.Name, r, (Color.White, Color.Black), Alignment.LEFT)
                Text.drawFillB(Style.baseFont, state.Scoring.State.Judgements.[i].ToString(), r, (Color.White, Color.Black), Alignment.RIGHT)
                r <- r.Translate(0.0f, h)

    type MultiplayerScoreTracker(conf: WidgetConfig.Pacemaker, state: PlayState) =
        inherit StaticWidget(NodeType.None)

        override this.Draw() =
            let x = this.Bounds.Right + 100.0f
            let mutable y = this.Bounds.Top
            Multiplayer.replays
            |> Seq.map (|KeyValue|)
            |> Seq.sortByDescending (fun (_, s) -> s.Value)
            |> Seq.iter (fun (username, s) ->
                let c = if username = Network.username then Color.SkyBlue else Color.White
                Text.draw(Style.baseFont, username, 20.0f, x, y, c)
                Text.drawJust(Style.baseFont, s.FormatAccuracy(), 20.0f, x - 10.0f, y, c, 1.0f)
                y <- y + 25.0f
            )

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            for s in Multiplayer.replays.Values do
                s.Update(state.CurrentChartTime() - Web.Shared.Packets.MULTIPLAYER_REPLAY_DELAY_MS * 2.0f<ms>)

    (*
        These widgets are configured by noteskin, not theme (and do not have positioning info)
    *)

    type ColumnLighting(keys, ns: NoteskinConfig, state) as this =
        inherit StaticWidget(NodeType.None)
        let sliders = Array.init keys (fun _ -> Animation.Fade 0.0f)
        let sprite = getTexture "receptorlighting"
        let lightTime = Math.Max(0.0f, Math.Min(0.99f, ns.ColumnLightTime))

        do
            let hitpos = float32 options.HitPosition.Value
            this.Position <- { Position.Default with Top = 0.0f %+ hitpos; Bottom = 1.0f %- hitpos }

        override this.Update(elapsedTime, bounds) =
            base.Update(elapsedTime, bounds)
            sliders |> Array.iter (fun s -> s.Update elapsedTime)
            Array.iteri (fun k (s: Animation.Fade) -> if state.Scoring.KeyState |> Bitmap.hasBit k then s.Value <- 1.0f) sliders

        override this.Draw() =
            let threshold = 1.0f - lightTime
            let f k (s: Animation.Fade) =
                if s.Value > threshold then
                    let p = (s.Value - threshold) / lightTime
                    let a = 255.0f * p |> int
                    Draw.sprite
                        (
                            let x = ns.ColumnWidth * 0.5f + (ns.ColumnWidth + ns.ColumnSpacing) * float32 k
                            if options.Upscroll.Value then
                                Sprite.alignedBoxX(this.Bounds.Left + x, this.Bounds.Top, 0.5f, 1.0f, ns.ColumnWidth * p, -1.0f / p) sprite
                            else Sprite.alignedBoxX(this.Bounds.Left + x, this.Bounds.Bottom, 0.5f, 1.0f, ns.ColumnWidth * p, 1.0f / p) sprite
                        )
                        (Color.FromArgb(a, Color.White))
                        sprite
            Array.iteri f sliders

    type Explosions(keys, ns: NoteskinConfig, state) as this =
        inherit StaticWidget(NodeType.None)

        let sliders = Array.init keys (fun _ -> Animation.Fade 0.0f)
        let timers = Array.zeroCreate keys
        let mem = Array.zeroCreate keys
        let holding = Array.create keys false
        let explodeTime = Math.Min(0.99f, ns.Explosions.FadeTime)
        let animation = Animation.Counter ns.Explosions.AnimationFrameTime
        let rotation = Noteskins.noteRotation keys

        let handleEvent (ev: HitEvent<HitEventGuts>) =
            match ev.Guts with
            | Hit e when (ns.Explosions.ExplodeOnMiss || not e.Missed) ->
                sliders.[ev.Column].Target <- 1.0f
                sliders.[ev.Column].Value <- 1.0f
                timers.[ev.Column] <- ev.Time
                holding.[ev.Column] <- true
                mem.[ev.Column] <- ev.Guts
            | Hit e when (ns.Explosions.ExplodeOnMiss || not e.Missed) ->
                sliders.[ev.Column].Value <- 1.0f
                timers.[ev.Column] <- ev.Time
                mem.[ev.Column] <- ev.Guts
            | _ -> ()

        do
            let hitpos = float32 options.HitPosition.Value
            this.Position <- { Position.Default with Top = 0.0f %+ hitpos; Bottom = 1.0f %- hitpos }
            state.Scoring.OnHit.Add handleEvent

        override this.Update(elapsedTime, moved) =
            base.Update(elapsedTime, moved)
            animation.Update elapsedTime
            sliders |> Array.iter (fun s -> s.Update elapsedTime)
            for k = 0 to (keys - 1) do
                if holding.[k] && state.Scoring.KeyState |> Bitmap.hasBit k |> not then
                    holding.[k] <- false
                    sliders.[k].Target <- 0.0f

        override this.Draw() =
            let columnwidth = ns.ColumnWidth
            let threshold = 1.0f - explodeTime
            let f k (s: Animation.Fade) =
                if s.Value > threshold then
                    let p = (s.Value - threshold) / explodeTime
                    let a = 255.0f * p |> int
                    
                    let box =
                        (
                            if options.Upscroll.Value then Rect.Box(this.Bounds.Left + (columnwidth + ns.ColumnSpacing) * float32 k, this.Bounds.Top, columnwidth, columnwidth)
                            else Rect.Box(this.Bounds.Left + (columnwidth + ns.ColumnSpacing) * float32 k, this.Bounds.Bottom - columnwidth, columnwidth, columnwidth)
                        )
                            .Expand((ns.Explosions.Scale - 1.0f) * columnwidth * 0.5f)
                            .Expand(ns.Explosions.ExpandAmount * (1.0f - p) * columnwidth, ns.Explosions.ExpandAmount * (1.0f - p) * columnwidth)
                    match mem.[k] with
                    | Hit e ->
                        let color = 
                            if ns.Explosions.Colors = ExplosionColors.Column then k
                            else match e.Judgement with Some j -> int j | None -> 0
                        let frame = (state.CurrentChartTime() - timers.[k]) / toTime ns.Explosions.AnimationFrameTime |> int
                        Draw.quad
                            (box |> Quad.ofRect |> rotation k)
                            (Quad.colorOf (Color.FromArgb(a, Color.White)))
                            (Sprite.gridUV (frame, color) (Content.getTexture (if e.IsHold then "holdexplosion" else "noteexplosion")))
                    | _ -> ()
            Array.iteri f sliders

    // Lane cover is controlled by game settings, not theme or noteskin

    type LaneCover() =
        inherit StaticWidget(NodeType.None)

        override this.Draw() =
            
            if options.LaneCover.Enabled.Value then

                let bounds = this.Bounds.Expand(0.0f, 2.0f)
                let fadeLength = float32 options.LaneCover.FadeLength.Value
                let upper (amount: float32) =
                    Draw.rect (bounds.SliceTop(amount - fadeLength)) options.LaneCover.Color.Value
                    Draw.quad
                        (bounds.SliceTop(amount).SliceBottom(fadeLength) |> Quad.ofRect)
                        struct (options.LaneCover.Color.Value, options.LaneCover.Color.Value, Color.FromArgb(0, options.LaneCover.Color.Value), Color.FromArgb(0, options.LaneCover.Color.Value))
                        Sprite.DefaultQuad
                let lower (amount: float32) =
                    Draw.rect (bounds.SliceBottom(amount - fadeLength)) options.LaneCover.Color.Value
                    Draw.quad
                        (bounds.SliceBottom(amount).SliceTop(fadeLength) |> Quad.ofRect)
                        struct (Color.FromArgb(0, options.LaneCover.Color.Value), Color.FromArgb(0, options.LaneCover.Color.Value), options.LaneCover.Color.Value, options.LaneCover.Color.Value)
                        Sprite.DefaultQuad

                let height = bounds.Height

                let sudden = float32 options.LaneCover.Sudden.Value * height
                let hidden = float32 options.LaneCover.Hidden.Value * height

                if options.Upscroll.Value then upper hidden; lower sudden
                else lower hidden; upper sudden