namespace Percyqaz.Flux.Windowing

type WindowType =
    | Windowed = 0
    | Borderless = 1
    | Fullscreen = 2
    | BorderlessNoTaskbar = 3
    | FullscreenLetterbox = 4

module WindowResolution =

    let PRESETS: (int * int) array =
        [|
            800, 600
            1024, 600
            1024, 768
            1152, 864
            1176, 664
            1280, 720
            1280, 768
            1280, 800
            1280, 960
            1280, 1024
            1360, 768
            1366, 768
            1440, 900
            1440, 1080
            1600, 900
            1600, 1024
            1680, 1050
            1920, 1080
            1920, 1200
            2560, 1080
            2560, 1440
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
        MSAASamples: int
    }