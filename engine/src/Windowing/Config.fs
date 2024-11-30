namespace Percyqaz.Flux.Windowing

open Percyqaz.Common

type WindowType =
    | Windowed = 0
    | Borderless = 1
    | Fullscreen = 2
    | BorderlessNoTaskbar = 3

module WindowResolution =

    let PRESETS: (int * int) array =
        [|
            800, 600
            1024, 768
            1280, 800
            1280, 1024
            1366, 768
            1600, 900
            1600, 1024
            1680, 1050
            1920, 1080
            2715, 1527
        |]

[<Struct; StructuralEquality; StructuralComparison>]
type FullscreenVideoMode =
    {
        Width: int
        Height: int
        RefreshRate: int
    }

type FrameLimit =
    | Unlimited = 0
    | Smart = 1

type WindowOptions =
    {
        WindowMode: WindowType
        WindowResolution: int * int
        FullscreenVideoMode: FullscreenVideoMode
        RenderMode: FrameLimit
        SmartCapAntiJitter: bool
        SmartCapFramerateMultiplier: float
        SmartCapTearlinePosition: float
        Display: int
        AudioDevice: int
        AudioDevicePeriod: int
        AudioDeviceBufferLengthMultiplier: int
        InputCPUSaver: bool
        EnableCursor: bool
    }

// todo: move out into Interlude startup logic
type WindowSettings =
    {
        WorkingDirectory: string
        WindowMode: Setting<WindowType>
        WindowResolution: Setting<int * int>
        FullscreenVideoMode: Setting<FullscreenVideoMode>
        RenderMode: Setting<FrameLimit>
        SmartCapAntiJitter: Setting<bool>
        SmartCapFramerateMultiplier: Setting<float>
        SmartCapTearlinePosition: Setting.Bounded<float>
        Display: Setting<int>
        AudioDevice: Setting<int>
        AudioDevicePeriod: Setting<int>
        AudioDeviceBufferLengthMultiplier: Setting<int>
        InputCPUSaver: Setting<bool>
    }

    static member Default =
        {
            WorkingDirectory = ""
            WindowMode = Setting.simple WindowType.Fullscreen
            WindowResolution = Setting.simple (1024, 768)
            FullscreenVideoMode =
                Setting.simple
                    {
                        Width = 1920
                        Height = 1080
                        RefreshRate = 60
                    }
            RenderMode = Setting.simple FrameLimit.Smart
            SmartCapAntiJitter = Setting.simple false
            SmartCapFramerateMultiplier = Setting.simple 8.0
            SmartCapTearlinePosition = Setting.percent 0.85
            Display = Setting.simple 0
            AudioDevice = Setting.simple -1
            AudioDevicePeriod = Setting.simple 2
            AudioDeviceBufferLengthMultiplier = Setting.simple 2
            InputCPUSaver = Setting.simple false
        }

    member this.ToOptions : WindowOptions =
        {
            WindowMode = this.WindowMode.Value
            WindowResolution = this.WindowResolution.Value
            FullscreenVideoMode = this.FullscreenVideoMode.Value
            RenderMode = this.RenderMode.Value
            SmartCapAntiJitter = this.SmartCapAntiJitter.Value
            SmartCapFramerateMultiplier = this.SmartCapFramerateMultiplier.Value
            SmartCapTearlinePosition = this.SmartCapTearlinePosition.Value
            Display = this.Display.Value
            AudioDevice = this.AudioDevice.Value
            AudioDevicePeriod = this.AudioDevicePeriod.Value
            AudioDeviceBufferLengthMultiplier = this.AudioDeviceBufferLengthMultiplier.Value
            InputCPUSaver = this.InputCPUSaver.Value
            EnableCursor = false
        }