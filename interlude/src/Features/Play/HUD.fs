namespace Interlude.Features.Play.HUD

open System
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Graphics
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Gameplay
open Prelude.Data.Content
open Interlude
open Interlude.UI
open Interlude.Content
open Interlude.Features
open Interlude.Features.Online
open Interlude.Features.Play
open Interlude.Features.Gameplay
open Interlude.Utils

(*
    Handful of widgets that directly pertain to gameplay
    They can all be toggled/repositioned/configured using themes
*)

type AccuracyMeter(conf: HUD.AccuracyMeter, state: PlayState) as this =
    inherit StaticContainer(NodeType.None)

    let grades = state.Ruleset.Grading.Grades

    let color =
        Animation.Color(
            if conf.GradeColors then
                Array.last(grades).Color
            else
                Color.White
        )

    do
        if conf.GradeColors then
            state.SubscribeToHits(fun _ ->
                color.Target <- Grade.calculate grades state.Scoring.State |> state.Ruleset.GradeColor
            )

        this
        |* Text(
            (fun () -> state.Scoring.FormatAccuracy()),
            Color = (fun () -> color.Value, Color.Transparent),
            Align = Alignment.CENTER,
            Position =
                { Position.Default with
                    Bottom = 0.7f %+ 0.0f
                }
        )

        if conf.ShowName then
            this
            |* Text(
                (fun () -> state.Scoring.Name),
                Color = K(Color.White, Color.Transparent),
                Align = Alignment.CENTER,
                Position =
                    { Position.Default with
                        Top = 0.6f %+ 0.0f
                    }
            )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms

[<Struct>]
type private HitMeterHit =
    {
        Time: Time
        Position: float32
        IsRelease: bool
        Judgement: JudgementId option
    }

type HitMeter(conf: HUD.HitMeter, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let hits = ResizeArray<HitMeterHit>()
    let mutable w = 0.0f

    let mutable last_seen_time = -Time.infinity
    let ln_mult = if conf.HalfScaleReleases then 0.5f else 1.0f
    let animation_time = conf.AnimationTime * Gameplay.rate.Value

    do
        state.SubscribeToHits(fun ev ->
            match ev.Guts with
            | Hit e ->
                hits.Add
                    {
                        Time = ev.Time
                        Position = e.Delta / state.Scoring.MissWindow * w * 0.5f
                        IsRelease = false
                        Judgement = e.Judgement
                    }
            | Release e ->
                hits.Add
                    {
                        Time = ev.Time
                        Position = e.Delta / state.Scoring.MissWindow * w * ln_mult
                        IsRelease = true
                        Judgement = e.Judgement
                    }
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if w = 0.0f then
            w <- this.Bounds.Width

        let now = state.CurrentChartTime()

        if now < last_seen_time then
            hits.Clear()

        last_seen_time <- now

        while hits.Count > 0 && hits.[0].Time + animation_time * 1.0f<ms> < now do
            hits.RemoveAt(0)

    override this.Draw() =
        let centre = this.Bounds.CenterX

        if conf.ShowGuide then
            Draw.rect
                (Rect.Create(centre - conf.Thickness, this.Bounds.Top, centre + conf.Thickness, this.Bounds.Bottom))
                Color.White

        let now = state.CurrentChartTime()

        for hit in hits do
            let r =
                Rect.Create(
                    centre + hit.Position - conf.Thickness,
                    this.Bounds.Top,
                    centre + hit.Position + conf.Thickness,
                    this.Bounds.Bottom
                )

            let c =
                match hit.Judgement with
                | None ->
                    Color.FromArgb(
                        Math.Clamp(127 - int (127.0f * (now - hit.Time) / animation_time), 0, 127),
                        Color.Silver
                    )
                | Some j ->
                    Color.FromArgb(
                        Math.Clamp(255 - int (255.0f * (now - hit.Time) / animation_time), 0, 255),
                        state.Ruleset.JudgementColor j
                    )

            if conf.ShowNonJudgements || hit.Judgement.IsSome then
                Draw.rect
                    (if hit.IsRelease then
                         r.Expand(0.0f, conf.ReleasesExtraHeight)
                     else
                         r)
                    c

type JudgementMeter(conf: HUD.JudgementMeter, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let atime = conf.AnimationTime * Gameplay.rate.Value * 1.0f<ms>
    let mutable tier = 0
    let mutable time = -Time.infinity

    do
        state.SubscribeToHits(fun ev ->
            let (judge, delta) =
                match ev.Guts with
                | Hit e -> (e.Judgement, e.Delta)
                | Release e -> (e.Judgement, e.Delta)

            if judge.IsSome && (not conf.IgnorePerfectJudgements || judge.Value > 0) then
                let j = judge.Value in

                if
                    not conf.PrioritiseLowerJudgements
                    || j >= tier
                    || ev.Time - atime > time
                    || ev.Time < time
                then
                    tier <- j
                    time <- ev.Time
        )

    override this.Draw() =
        if time > -Time.infinity then
            let a =
                255
                - Math.Clamp(255.0f * (state.CurrentChartTime() - time) / atime |> int, 0, 255)

            Text.fill (
                Style.font,
                state.Ruleset.JudgementName tier,
                this.Bounds,
                state.Ruleset.JudgementColor(tier).O4a a,
                Alignment.CENTER
            )

type EarlyLateMeter(conf: HUD.EarlyLateMeter, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let atime = conf.AnimationTime * rate.Value * 1.0f<ms>
    let mutable early = false
    let mutable time = -Time.infinity

    do
        state.SubscribeToHits(fun ev ->
            let (judge, delta) =
                match ev.Guts with
                | Hit e -> (e.Judgement, e.Delta)
                | Release e -> (e.Judgement, e.Delta)

            if judge.IsSome && judge.Value > 0 then
                early <- delta < 0.0f<ms>
                time <- ev.Time
        )

    override this.Draw() =
        if time > -Time.infinity then
            let a =
                255
                - Math.Clamp(255.0f * (state.CurrentChartTime() - time) / atime |> int, 0, 255)

            Text.fill (
                Style.font,
                (if early then conf.EarlyText else conf.LateText),
                this.Bounds,
                (if early then conf.EarlyColor else conf.LateColor).O4a a,
                Alignment.CENTER
            )

type ComboMeter(conf: HUD.Combo, state: PlayState) =
    inherit StaticWidget(NodeType.None)
    let pop_animation = Animation.Fade(0.0f)
    let color = Animation.Color(Color.White)
    let mutable hits = 0

    do
        state.SubscribeToHits(fun _ ->
            hits <- hits + 1

            if (conf.LampColors && hits > 50) then
                color.Target <-
                    Lamp.calculate state.Ruleset.Grading.Lamps state.Scoring.State
                    |> state.Ruleset.LampColor

            pop_animation.Value <- conf.Pop
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        color.Update elapsed_ms
        pop_animation.Update elapsed_ms

    override this.Draw() =
        let combo = state.Scoring.State.CurrentCombo

        let amt =
            pop_animation.Value + (((combo, 1000) |> Math.Min |> float32) * conf.Growth)

        Text.fill (Style.font, combo.ToString(), this.Bounds.Expand amt, color.Value, 0.5f)

type ProgressMeter(conf: HUD.ProgressMeter, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let duration =
        let chart = state.WithMods
        chart.LastNote - chart.FirstNote

    override this.Draw() =
        let now = state.CurrentChartTime()
        let pc = now / duration |> max 0.0f |> min 1.0f

        let x, y = this.Bounds.Center
        let r = (min this.Bounds.Width this.Bounds.Height) * 0.5f
        let angle = MathF.PI / 15.0f

        let outer i =
            let angle = float32 i * angle
            let struct (a, b) = MathF.SinCos(angle)
            (x + r * a, y - r * b)

        let inner i =
            let angle = float32 i * angle
            let struct (a, b) = MathF.SinCos(angle)
            (x + (r - 4f) * a, y - (r - 4f) * b)

        for i = 0 to 29 do
            Draw.untextured_quad
                (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1)))
                (Quad.color conf.BackgroundColor)

            Draw.untextured_quad
                (Quad.createv (inner i) (outer i) (outer (i + 1)) (inner (i + 1)))
                (Quad.color Colors.white.O2)

        for i = 0 to pc * 29.9f |> floor |> int do
            Draw.untextured_quad (Quad.createv (x, y) (x, y) (inner i) (inner (i + 1))) (Quad.color conf.Color)

        let text =
            match conf.Label with
            | HUD.ProgressMeterLabel.Countdown ->
                let time_left = (duration - now) / Gameplay.rate.Value

                sprintf
                    "%i:%02i"
                    (time_left / 60000.0f<ms> |> floor |> int)
                    ((time_left % 60000.0f<ms>) / 1000.0f<ms> |> floor |> int)
            | HUD.ProgressMeterLabel.Percentage -> sprintf "%.0f%%" (pc * 100.0f)
            | _ -> ""

        Text.fill_b (
            Style.font,
            text,
            this.Bounds.Expand(0.0f, 40.0f).SliceBottom(40.0f),
            Colors.text_subheading,
            Alignment.CENTER
        )

type SkipButton(conf: HUD.SkipButton, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let text = [ (%%"skip").ToString() ] %> "play.skiphint"
    let mutable active = true

    let first_note = state.WithMods.FirstNote

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if active && state.CurrentChartTime() < -Song.LEADIN_TIME * 2.5f then
            if (%%"skip").Tapped() then
                Song.pause ()
                Song.play_from (first_note - Song.LEADIN_TIME)
        else
            active <- false

    override this.Draw() =
        if active then
            Text.fill_b (Style.font, text, this.Bounds, Colors.text, Alignment.CENTER)

type Pacemaker(conf: HUD.Pacemaker, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let color = Animation.Color(Color.White)
    let flag_position = Animation.Fade(0.5f)
    let position_cooldown = Animation.Delay(3000.0)
    let mutable ahead_by = 0.0
    let mutable hearts = -1

    let update_flag_position () =
        if ahead_by >= 10.0 then
            flag_position.Target <- 1.0f
        elif ahead_by > -10.0 then
            flag_position.Target <- (ahead_by + 10.0) / 20.0 |> float32

            if ahead_by > 0.0 then
                color.Target <- Color.FromHsv(140.0f / 360.0f, ahead_by / 10.0 |> float32, 1.0f)
            else
                color.Target <- Color.FromHsv(340.0f / 360.0f, ahead_by / -10.0 |> float32, 1.0f)
        else
            flag_position.Target <- 0.0f

    do
        match state.Pacemaker with
        | PacemakerInfo.None
        | PacemakerInfo.Accuracy _
        | PacemakerInfo.Replay _ -> ()
        | PacemakerInfo.Judgement(judgement, _) ->
            color.Target <-
                if judgement = -1 then
                    Rulesets.current.Judgements.[Rulesets.current.Judgements.Length - 1].Color
                else
                    Rulesets.current.Judgements.[judgement].Color


    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        match state.Pacemaker with
        | PacemakerInfo.None -> ()
        | PacemakerInfo.Accuracy x ->
            if position_cooldown.Complete then
                ahead_by <- state.Scoring.State.PointsScored - state.Scoring.State.MaxPointsScored * x
                update_flag_position ()
                position_cooldown.Reset()

            flag_position.Update elapsed_ms
            position_cooldown.Update elapsed_ms
        | PacemakerInfo.Replay score ->
            if position_cooldown.Complete then
                score.Update(state.CurrentChartTime())
                ahead_by <- state.Scoring.State.PointsScored - score.State.PointsScored
                update_flag_position ()
                position_cooldown.Reset()

            flag_position.Update elapsed_ms
            position_cooldown.Update elapsed_ms
        | PacemakerInfo.Judgement(_, _) -> ()

        color.Update elapsed_ms

    override this.Draw() =
        match state.Pacemaker with
        | PacemakerInfo.None -> ()
        | PacemakerInfo.Accuracy _
        | PacemakerInfo.Replay _ ->
            Text.fill_b (
                Style.font,
                Icons.FLAG,
                this.Bounds
                    .SliceLeft(0.0f)
                    .Expand(this.Bounds.Height, 0.0f)
                    .Translate(this.Bounds.Width * flag_position.Value, 0.0f),
                (color.Value, Color.Black),
                Alignment.CENTER
            )
        | PacemakerInfo.Judgement(judgement, count) ->
            let actual =
                if judgement = -1 then
                    state.Scoring.State.ComboBreaks
                else
                    let mutable c = state.Scoring.State.Judgements.[judgement]

                    for j = judgement + 1 to state.Scoring.State.Judgements.Length - 1 do
                        if state.Scoring.State.Judgements.[j] > 0 then
                            c <- 1000000

                    c

            let _hearts = 1 + count - actual

            if _hearts < hearts then
                color.Value <- Color.White

            hearts <- _hearts

            let display =
                if hearts > 5 then
                    sprintf "%s x%i" (String.replicate 5 Icons.HEART_ON) hearts
                elif hearts > 0 then
                    (String.replicate hearts Icons.HEART_ON)
                else
                    Icons.X

            Text.fill_b (Style.font, display, this.Bounds, (color.Value, Color.Black), Alignment.CENTER)

type JudgementCounts(conf: HUD.JudgementCounts, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    let judgement_animations =
        Array.init state.Ruleset.Judgements.Length (fun _ -> Animation.Delay(conf.AnimationTime))

    do
        state.SubscribeToHits(fun h ->
            match h.Guts with
            | Hit x ->
                if x.Judgement.IsSome then
                    judgement_animations[x.Judgement.Value].Reset()
            | Release x ->
                if x.Judgement.IsSome then
                    judgement_animations[x.Judgement.Value].Reset()
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for j in judgement_animations do
            j.Update elapsed_ms

    override this.Draw() =
        let h = this.Bounds.Height / float32 judgement_animations.Length
        let mutable r = this.Bounds.SliceTop(h).Shrink(5.0f)

        for i = 0 to state.Ruleset.Judgements.Length - 1 do
            let j = state.Ruleset.Judgements.[i]
            Draw.rect (r.Expand(10.0f, 5.0f).SliceLeft(5.0f)) j.Color

            if not judgement_animations.[i].Complete && state.Scoring.State.Judgements.[i] > 0 then
                Draw.rect
                    (r.Expand 5.0f)
                    (Color.FromArgb(
                        127
                        - max 0 (int (127.0 * judgement_animations.[i].Elapsed / conf.AnimationTime)),
                        j.Color
                    ))

            Text.fill_b (Style.font, j.Name, r, (Color.White, Color.Black), Alignment.LEFT)

            Text.fill_b (
                Style.font,
                state.Scoring.State.Judgements.[i].ToString(),
                r,
                (Color.White, Color.Black),
                Alignment.RIGHT
            )

            r <- r.Translate(0.0f, h)

// todo: give this thing its own placement config + config on your username color vs other peoples
type MultiplayerScoreTracker(conf: HUD.Pacemaker, state: PlayState) =
    inherit StaticWidget(NodeType.None)

    override this.Draw() =
        let x = this.Bounds.Right + 100.0f
        let mutable y = this.Bounds.Top

        Multiplayer.replays
        |> Seq.map (|KeyValue|)
        |> Seq.sortByDescending (fun (_, (s, _)) -> s.Value)
        |> Seq.iter (fun (username, (s, _)) ->
            let c =
                if username = Network.credentials.Username then
                    Color.SkyBlue
                else
                    Color.White

            Text.draw (Style.font, username, 20.0f, x, y, c)
            Text.draw_aligned (Style.font, s.FormatAccuracy(), 20.0f, x - 10.0f, y, c, 1.0f)
            y <- y + 25.0f
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        for s, _ in Multiplayer.replays.Values do
            s.Update(
                state.CurrentChartTime()
                - Web.Shared.Packets.MULTIPLAYER_REPLAY_DELAY_MS * 2.0f<ms>
            )

type RateModMeter(conf: HUD.RateModMeter, state: PlayState) as this =
    inherit StaticContainer(NodeType.None)

    do
        let text =
            if conf.ShowMods then
                Mods.format_mods (Gameplay.rate.Value, Gameplay.selected_mods.Value, Gameplay.autoplay)
            else
                sprintf "%.2fx" Gameplay.rate.Value

        this |* Text(text, Color = K Colors.text_subheading, Align = Alignment.CENTER)

type BPMMeter(conf: HUD.BPMMeter, state: PlayState) as this =
    inherit StaticContainer(NodeType.None)

    let first_note = state.WithMods.FirstNote
    let mutable i = 0
    let bpms = state.WithMods.BPM
    let mutable last_seen_time = -Time.infinity

    do
        this
        |* Text(
            (fun () ->
                let ms_per_beat = bpms.[i].Data.MsPerBeat / Gameplay.rate.Value in
                sprintf "%.0f BPM" (60000.0f<ms / minute> / ms_per_beat)
            ),
            Color = K Colors.text_subheading,
            Align = Alignment.CENTER
        )

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        let now = state.CurrentChartTime()

        if now < last_seen_time then
            i <- 0

        last_seen_time <- now

        while i + 1 < bpms.Length && (bpms[i + 1].Time - first_note) < now do
            i <- i + 1
