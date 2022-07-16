namespace Interlude.UI.OptionsMenu

open OpenTK
open Percyqaz.Common
open Percyqaz.Flux.Audio
open Prelude.Common
open Interlude.Options
open Interlude.UI.Components.Selection
open Interlude.UI.Components.Selection.Controls
open Interlude.UI.Components.Selection.Menu

module System =

    let monitors() =
        seq {
            for i, m in Seq.indexed (Windowing.Desktop.Monitors.GetMonitors()) do
                yield i, sprintf "%i: %s" (i + 1) m.Name
        } |> Array.ofSeq
    
    let page() : SelectionPage =
        {
            Content = fun add ->
                column [
                    PrettySetting("system.visualoffset", new Slider<float>(options.VisualOffset, 0.01f)).Position(200.0f)

                    PrettySetting("system.audiooffset",
                        { new Slider<float>(options.AudioOffset, 0.01f)
                            with override this.OnDeselect() = Song.changeGlobalOffset (float32 options.AudioOffset.Value * 1.0f<ms>) }
                    ).Position(280.0f)

                    PrettySetting("system.audiovolume",
                        Slider<_>.Percent(options.AudioVolume |> Setting.trigger Devices.changeVolume, 0.01f)
                    ).Position(380.0f)
                    PrettySetting("system.audiodevice", Selector(Array.ofSeq(Devices.list()), Setting.trigger Devices.change config.AudioDevice)).Position(460.0f, 1700.0f)

                    PrettySetting("system.windowmode", Selector.FromEnum config.WindowMode).Position(560.0f)
                    // todo: way to edit resolution settings?
                    PrettySetting("system.framelimit", Selector.FromEnum config.FrameLimit).Position(640.0f)
                    PrettySetting("system.monitor", Selector(monitors(), config.Display)).Position(720.0f)
                ] :> Selectable
            Callback = applyOptions
        }