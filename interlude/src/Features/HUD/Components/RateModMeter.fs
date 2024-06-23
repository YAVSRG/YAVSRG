namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Gameplay.Mods
open Prelude.Skinning.Noteskins
open Interlude.Features.Play
open Interlude.Features.Gameplay

type RateModMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) =
    inherit Container(NodeType.None)

    override this.Init(parent) =
        let text =
            if user_options.RateModMeterShowMods then
                Mods.format (SelectedChart.rate.Value, state.WithColors.ModsSelected, SelectedChart.autoplay)
            else
                sprintf "%.2fx" SelectedChart.rate.Value

        this |* Text(text, Color = K Colors.text_subheading, Align = Alignment.CENTER)
        base.Init parent