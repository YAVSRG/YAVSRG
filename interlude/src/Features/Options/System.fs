namespace Interlude.Features.OptionsMenu

open Percyqaz.Common
open Percyqaz.Flux.Audio
open Percyqaz.Flux.Windowing
open Percyqaz.Flux.UI
open Prelude.Common
open Interlude.Options
open Interlude.UI.Menu

module System =

    type SystemPage() as this =
        inherit Page()

        do
            this.Content(
                column()
                |+ PrettySetting("system.visualoffset", Slider<float>(options.VisualOffset, 0.01f)).Pos(200.0f)
                |+ PrettySetting("system.audiooffset",
                        { new Slider<float>(options.AudioOffset, 0.01f)
                            with override this.OnDeselected() = base.OnDeselected(); Song.changeGlobalOffset (float32 options.AudioOffset.Value * 1.0f<ms>) }
                    ).Pos(280.0f)

                |+ PrettySetting("system.audiovolume",
                        Slider<_>.Percent(options.AudioVolume |> Setting.trigger Devices.changeVolume, 0.01f)
                    ).Pos(380.0f)
                |+ PrettySetting("system.audiodevice", Selector(Array.ofSeq(Devices.list()), Setting.trigger Devices.change config.AudioDevice)).Pos(460.0f, 1700.0f)

                // todo: way to edit resolution settings?
                |+ PrettySetting("system.windowmode", Selector.FromEnum config.WindowMode).Pos(560.0f)
                |+ PrettySetting("system.framelimit", Selector.FromEnum config.FrameLimit).Pos(640.0f)
                |+ PrettySetting("system.monitor", Selector(Window.monitors, config.Display)).Pos(720.0f)
            )

        override this.OnClose() = Window.apply_config <- Some config
        override this.Title = N"system"