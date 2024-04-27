namespace Interlude.Features.Play.HUD

open Percyqaz.Common
open Percyqaz.Flux.UI
open Prelude.Gameplay.Mods
open Prelude.Skinning.Noteskins
open Interlude.Features.Play
open Interlude.Features.Gameplay

type RateModMeter(user_options: HUDUserOptions, noteskin_options: HUDNoteskinOptions, state: PlayState) as this =
    inherit Container(NodeType.None)

    do
        let text =
            if user_options.RateModMeterShowMods then
                Mods.format (rate.Value, state.WithColors.ModsSelected, autoplay)
            else
                sprintf "%.2fx" rate.Value

        this |* Text(text, Color = K Colors.text_subheading, Align = Alignment.CENTER)