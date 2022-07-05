namespace Percyqaz.Flux.Windowing

open Percyqaz.Common

type WindowType =
    | Windowed = 0
    | Borderless = 1
    | Fullscreen = 2
    | ``Borderless Fullscreen`` = 3

type WindowResolution =
    | Preset of index:int
    | Custom of width:int * height:int
    static member Presets : (int * int) array =
            [|(800, 600); (1024, 768); (1280, 800); (1280, 1024); (1366, 768); (1600, 900);
                (1600, 1024); (1680, 1050); (1920, 1080); (2715, 1527)|]
    member this.Dimensions : int * int * bool =
        match this with
        | Custom (w, h) -> w, h, true
        | Preset i ->
            let resolutions = WindowResolution.Presets
            let i = System.Math.Clamp(i, 0, Array.length resolutions - 1)
            let w, h = resolutions.[i]
            w, h, false

type FrameLimit =
    | ``30`` = 30
    | ``60`` = 60
    | ``120`` = 120
    | ``240`` = 240
    | ``480`` = 480
    | Unlimited = 0

type Config = 
    {
        WorkingDirectory: string
        Locale: string
        WindowMode: Setting<WindowType>
        Resolution: Setting<WindowResolution>
        FrameLimit: Setting<FrameLimit>
        Display: Setting<int>
        AudioDevice: Setting<int>
    }
    static member Default = 
        {
            WorkingDirectory = ""
            Locale = "en_GB.txt"
            WindowMode = Setting.simple WindowType.Borderless
            Resolution = Setting.simple (Custom (1024, 768))
            FrameLimit = Setting.simple FrameLimit.``480``
            Display = Setting.simple 0
            AudioDevice = Setting.simple -1
        }