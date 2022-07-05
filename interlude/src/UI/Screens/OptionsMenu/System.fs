namespace Interlude.UI.OptionsMenu

open OpenTK
open Percyqaz.Common
open Prelude.Common
open Interlude
open Interlude.Options
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module System =

    let monitors() =
        seq {
            for i in 0 .. Windowing.Desktop.Monitors.Count - 1 do
                let ok, info = Windowing.Desktop.Monitors.TryGetMonitorInfo i
                if ok then yield i, sprintf "Monitor %i" (i + 1)
        } |> Array.ofSeq
    
    let page() : SelectionPage =
        {
            Content = fun add ->
                column [
                    PrettySetting("system.visualoffset", new Slider<float>(options.VisualOffset, 0.01f)).Position(200.0f)

                    PrettySetting("system.audiooffset",
                        { new Slider<float>(options.AudioOffset, 0.01f)
                            with override this.OnDeselect() = Audio.changeGlobalOffset (float32 options.AudioOffset.Value * 1.0f<ms>) }
                    ).Position(280.0f)

                    PrettySetting("system.audiovolume",
                        Slider<_>.Percent(options.AudioVolume |> Setting.trigger Audio.changeVolume, 0.01f)
                    ).Position(380.0f)
                    PrettySetting("system.audiodevice", Selector(Audio.devices, Setting.trigger Audio.changeDevice config.AudioDevice)).Position(460.0f, 1700.0f)

                    PrettySetting("system.windowmode", Selector.FromEnum config.WindowMode).Position(560.0f)
                    // todo: way to edit resolution settings?
                    PrettySetting("system.framelimit", Selector.FromEnum config.FrameLimit).Position(640.0f)
                    PrettySetting("system.monitor", Selector(monitors(), config.Display)).Position(720.0f)
                ] :> Selectable
            Callback = applyOptions
        }