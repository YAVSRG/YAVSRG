namespace Interlude.Features.Score

open Percyqaz.Flux.Graphics
open Percyqaz.Flux.UI
open Prelude
open Prelude.Data.User.Stats
open Interlude.UI

type SessionScoreBar(xp_gain: SessionXPGain) =
    inherit StaticWidget(NodeType.None)

    let xp_now = CURRENT_SESSION.SessionScore
    let xp_before = xp_now - xp_gain.Total

    let xp_display = Animation.Fade(float32 xp_before)

    override this.Init(parent: Widget) =
        printfn "%A" xp_gain
        printfn "%i -> %i" xp_before xp_now

        if xp_gain.QuitPenalty <> 0L then
            ScoreScreenHelpers.animation_queue.Add (Animation.Delay 1000.0)
            ScoreScreenHelpers.animation_queue.Add (Animation.Action (fun () -> xp_display.Target <- xp_display.Target + float32 xp_gain.QuitPenalty))
        if xp_gain.BaseXP <> 0L then
            ScoreScreenHelpers.animation_queue.Add (Animation.Delay 1000.0)
            ScoreScreenHelpers.animation_queue.Add (Animation.Action (fun () -> xp_display.Target <- xp_display.Target + float32 xp_gain.BaseXP))
        if xp_gain.AccXP <> 0L then
            ScoreScreenHelpers.animation_queue.Add (Animation.Delay 1000.0)
            ScoreScreenHelpers.animation_queue.Add (Animation.Action (fun () -> xp_display.Target <- xp_display.Target + float32 xp_gain.AccXP))
        if xp_gain.LampXP <> 0L then
            ScoreScreenHelpers.animation_queue.Add (Animation.Delay 1000.0)
            ScoreScreenHelpers.animation_queue.Add (Animation.Action (fun () -> xp_display.Target <- xp_display.Target + float32 xp_gain.LampXP))
        if xp_gain.SkillXP <> 0L then
            ScoreScreenHelpers.animation_queue.Add (Animation.Delay 1000.0)
            ScoreScreenHelpers.animation_queue.Add (Animation.Action (fun () -> xp_display.Target <- xp_display.Target + float32 xp_gain.SkillXP))
        ScoreScreenHelpers.animation_queue.Add (Animation.Action(fun () -> xp_display.Target <- float32 xp_now))

        base.Init parent

    override this.Draw() =
        let level = int64 xp_display.Value |> current_level
        let xp_start = xp_for_level level |> float32
        let xp_end = xp_for_level (level + 1) |> float32
        let xp_progress = (xp_display.Value - xp_start) / (xp_end - xp_start)

        // box
        Render.rect (this.Bounds.Translate(10.0f, 10.0f)) Colors.black
        Background.draw (this.Bounds, (Color.FromArgb(40, 40, 40)), 2.0f)
        Render.rect this.Bounds (!*Palette.MAIN_100)

        // bar
        let bar = this.Bounds.SliceY(20.0f).ShrinkX(10.0f)
        Render.rect bar Colors.shadow_2.O2
        Render.rect (bar.SlicePercentL(xp_progress)) (!*Palette.HIGHLIGHT_100).O4

        let counter = this.Bounds.BorderB(60.0f).SliceR(220.0f)
        let counterq =
            let q = counter.AsQuad
            { q with TopLeft = q.TopLeft - OpenTK.Mathematics.Vector2(30.0f, 0.0f) }
        Render.quad (Quad.translate (10.0f, 10.0f) counterq) Colors.black
        Background.drawq (counterq, (Color.FromArgb(40, 40, 40)), 2.0f)
        Render.quad counterq (!*Palette.MAIN_100)

        Text.fill_b(Style.font, sprintf "%.0f" xp_display.Value, counter, Colors.text, Alignment.CENTER)
        Text.fill_b(Style.font, sprintf "Level %i" level, counter.BorderB(40.0f).TranslateY(10.0f), Colors.text, Alignment.CENTER)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)
        xp_display.Update elapsed_ms