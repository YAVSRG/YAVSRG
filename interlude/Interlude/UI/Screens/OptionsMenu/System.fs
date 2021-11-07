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
                    //todo: way to edit resolution settings?
                    PrettySetting(
                        "FrameLimiter",
                        new Selector(
                            [|"UNLIMITED"; "30"; "60"; "90"; "120"; "240"|],
                            config.FrameLimiter.Value / 30.0
                            |> int
                            |> min 5
                            |> Setting.simple
                            |> Setting.trigger
                                (let e = [|0.0; 30.0; 60.0; 90.0; 120.0; 240.0|] in fun i -> config.FrameLimiter.Value <- e.[i]) )
                    ).Position(500.0f)
                ] :> Selectable
            Callback = Options.applyOptions
        }