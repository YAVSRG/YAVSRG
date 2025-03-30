namespace Percyqaz.Flux.Windowing

type WindowType =
    | Windowed = 0
    | Borderless = 1
    | Fullscreen = 2
    | BorderlessNoTaskbar = 3
    | FullscreenLetterbox = 4

type WindowedResolution = (int * int) * (float32 * float32)
module WindowedResolution =

    let DEFAULT: WindowedResolution = (1024, 768), (0.5f, 0.5f)

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

[<Struct>]
type FullscreenVideoMode =
    {
        Width: int
        Height: int
        RefreshRate: int
    }

type FrameLimit =
    | Unlimited = 0
    | Smart = 1
    | Custom = 2

type WindowOptions =
    {
        WindowMode: WindowType
        WindowedResolution: WindowedResolution
        FullscreenVideoMode: FullscreenVideoMode
        RenderMode: FrameLimit
        CustomFrameLimit: int
        SmartCapAntiJitter: bool
        SmartCapFramerateMultiplier: float
        SmartCapTearlinePosition: float
        Display: int
        AudioDevice: int
        AudioDevicePeriod: int
        AudioDeviceBufferLengthMultiplier: int
        InputCPUSaver: bool
        HideCursor: bool
        MSAASamples: int
    }