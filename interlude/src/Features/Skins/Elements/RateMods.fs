namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Mods
open Prelude.Skins.HudLayouts
open Interlude.Features.Play
open Interlude.Features.Gameplay

type RateMods(ctx: HudContext) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        let text =
            if ctx.Config.RateModMeterShowMods then
                ModState.format (SelectedChart.rate.Value, ctx.State.WithColors.ModsSelected)
            else
                sprintf "%.2fx" SelectedChart.rate.Value

        this |* Text(text)
            .Color(Colors.text_subheading)
            .Align(Alignment.CENTER)
        base.Init parent