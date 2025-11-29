namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.Audio
open Percyqaz.Flux.Input
open Percyqaz.Flux.UI
open Prelude
open Prelude.Skins.HudLayouts
open Interlude.Content
open Interlude.Features.Gameplay
open Interlude.Features.Play

type SkipButton(ctx: HudContext) =
    inherit Container(NodeType.None)

    let SKIP_THRESHOLD = 1500.0f<ms / rate> * SelectedChart.rate.Value
    let SKIP_DISTANCE = 1000.0f<ms / rate> * SelectedChart.rate.Value

    let text = [ (%%"skip").ToString() ] %> "play.skiphint"
    let mutable active = true

    override this.Init(parent: Widget) =
        let background = ctx.Config.SkipButtonBackground
        if background.Enable then
            let lo = (1.0f - background.Scale) * 0.5f
            let hi = 1.0f - lo
            this.Add(
                Image(Content.Texture "skip-button-bg", StretchToFill = false)
                    .Position(
                        {
                            Left = (lo - 0.5f + background.AlignmentX) %+ 0.0f
                            Top = (lo - 0.5f + background.AlignmentY) %+ 0.0f
                            Right = (hi - 0.5f + background.AlignmentX) %+ 0.0f
                            Bottom = (hi - 0.5f + background.AlignmentY) %+ 0.0f
                        }
                    )
            )
        this.Add(Text(text).Color(Colors.text).Align(Alignment.CENTER))
        base.Init(parent)

    override this.Update(elapsed_ms, moved) =
        base.Update(elapsed_ms, moved)

        if active && ctx.Inner.IsPlay || ctx.Inner.IsReplay then

            if ctx.State.CurrentChartTime() < -SKIP_THRESHOLD then
                if (%%"skip").Pressed() then
                    Song.pause ()
                    Song.play_from (ctx.State.WithColors.FirstNote - SKIP_DISTANCE)
                    active <- false
            else
                active <- false

    override this.Draw() =
        if active then
            base.Draw()