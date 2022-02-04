namespace Interlude.UI.OptionsMenu

open OpenTK
open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module System =
    
    let page() : SelectionPage =
        {
            Content = fun add ->
                column [
                    PrettySetting("system.visualoffset", new Slider<float>(options.VisualOffset, 0.01f)).Position(200.0f)

                    PrettySetting("system.audiooffset",
                        { new Slider<float>(options.AudioOffset, 0.01f)
                            with override this.OnDeselect() = Audio.changeGlobalOffset (float32 options.AudioOffset.Value * 1.0f<ms>) }
                    ).Position(300.0f)

                    PrettySetting("system.audiovolume",
                        Slider<_>.Percent(options.AudioVolume |> Setting.trigger Audio.changeVolume, 0.01f)
                    ).Position(400.0f)

                    PrettySetting("system.windowmode", Selector.FromEnum config.WindowMode).Position(500.0f)
                    // todo: way to edit resolution settings?
                    PrettySetting("system.framelimit", Selector.FromEnum config.FrameLimit).Position(600.0f)
                ] :> Selectable
            Callback = applyOptions
        }