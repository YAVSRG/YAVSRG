﻿namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Gameplay.Mods
open Prelude.Skinning.HudLayouts
open Interlude.Features.Play
open Interlude.Features.Gameplay

type RateModMeter(config: HudConfig, state: PlayState) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        let text =
            if config.RateModMeterShowMods then
                Mods.format (SelectedChart.rate.Value, state.WithColors.ModsSelected, SelectedChart.autoplay)
            else
                sprintf "%.2fx" SelectedChart.rate.Value

        this |* Text(text, Color = K Colors.text_subheading, Align = Alignment.CENTER)
        base.Init parent