namespace Interlude.UI.OptionsMenu

open OpenTK
open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module System =
    
    let icon = "❖"
    let page() : SelectionPage =
        {
            Content = fun add ->
                column [
                    PrettySetting("AudioOffset",
                        { new Slider<float>(options.AudioOffset, 0.01f)
                            with override this.OnDeselect() = Audio.globalOffset <- float32 options.AudioOffset.Value * 1.0f<ms> }
                    ).Position(200.0f)

                    PrettySetting("AudioVolume",
                        new Slider<float>(options.AudioVolume |> Setting.trigger Audio.changeVolume, 0.01f)
                    ).Position(300.0f)

                    PrettySetting("WindowMode", Selector.FromEnum(config.WindowMode)).Position(400.0f)
                    // todo: way to edit resolution settings?
                    PrettySetting("FrameLimiter", Selector.FromEnum(config.FrameLimit)).Position(500.0f)
                ] :> Selectable
            Callback = applyOptions
        }