namespace Interlude.Features.Play.HUD

open Percyqaz.Flux.UI
open Prelude.Mods
open Prelude.Skins.HudLayouts
open Interlude.Features.Play

type RateMods(ctx: HudContext) =
    inherit Container(NodeType.None)

    override this.Init(parent: Widget) : unit =
        let update_text() : string =
            if ctx.Config.RateModMeterShowMods then
                ModState.format (ctx.State.Scoring.Rate, ctx.State.WithColors.ModsSelected)
            else
                sprintf "%.2fx" ctx.State.Scoring.Rate
                
        let mutable text = update_text()
        ctx.State.OnScoringChanged(fun () -> text <- update_text()) |> ignore

        this.Add(
            Text(fun () -> text)
                .Color(Colors.text_subheading)
                .Align(Alignment.CENTER)
        )
        base.Init(parent)