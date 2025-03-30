namespace Interlude.Options

open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Flux.Windowing
open Prelude

[<Json.AutoCodec(false)>]
type WindowSettings =
    {
        WorkingDirectory: string
        WindowMode: Setting<WindowType>
        WindowedResolution: Setting<WindowedResolution>
        FullscreenVideoMode: Setting<FullscreenVideoMode>
        RenderMode: Setting<FrameLimit>
        CustomFrameLimit : Setting.Bounded<int>
        SmartCapAntiJitter: Setting<bool>
        SmartCapFramerateMultiplier: Setting<float>
        SmartCapTearlinePosition: Setting.Bounded<float>
        Display: Setting<int>
        AudioDevice: Setting<int>
        AudioDevicePeriod: Setting<int>
        AudioDeviceBufferLengthMultiplier: Setting<int>
        InputCPUSaver: Setting<bool>
        MSAASamples: Setting<int>
    }

    static member Default =
        {
            WorkingDirectory = ""
            WindowMode = Setting.simple WindowType.BorderlessNoTaskbar
            WindowedResolution = Setting.simple WindowedResolution.DEFAULT
            FullscreenVideoMode =
                Setting.simple
                    {
                        Width = 1920
                        Height = 1080
                        RefreshRate = 60
                    }
            RenderMode = Setting.simple FrameLimit.Smart
            CustomFrameLimit = Setting.bounded (30, 5000) 1000
            SmartCapAntiJitter = Setting.simple false
            SmartCapFramerateMultiplier = Setting.simple 8.0
            SmartCapTearlinePosition = Setting.percent 0.85
            Display = Setting.simple 0
            AudioDevice = Setting.simple -1
            AudioDevicePeriod = Setting.simple 2
            AudioDeviceBufferLengthMultiplier = Setting.simple 2
            InputCPUSaver = Setting.simple true
            MSAASamples = Setting.simple 4
        }

    member this.ToOptions : WindowOptions =
        {
            WindowMode = this.WindowMode.Value
            WindowedResolution = this.WindowedResolution.Value
            FullscreenVideoMode = this.FullscreenVideoMode.Value
            RenderMode = this.RenderMode.Value
            CustomFrameLimit = this.CustomFrameLimit.Value
            SmartCapAntiJitter = this.SmartCapAntiJitter.Value
            SmartCapFramerateMultiplier = this.SmartCapFramerateMultiplier.Value
            SmartCapTearlinePosition = this.SmartCapTearlinePosition.Value
            Display = this.Display.Value
            AudioDevice = this.AudioDevice.Value
            AudioDevicePeriod = this.AudioDevicePeriod.Value
            AudioDeviceBufferLengthMultiplier = this.AudioDeviceBufferLengthMultiplier.Value
            InputCPUSaver = this.InputCPUSaver.Value
            HideCursor = true
            MSAASamples = this.MSAASamples.Value
        }

    member this.Apply () =
        let as_options = this.ToOptions
        WindowThread.defer (fun () -> WindowThread.apply_config as_options)