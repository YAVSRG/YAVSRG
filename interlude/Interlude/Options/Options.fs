namespace Interlude

open System
open System.Collections.Generic
open System.IO
open OpenTK.Input
open Prelude.Common
open Prelude.Json
open Prelude.Gameplay.Score
open Prelude.Gameplay.Layout
open Prelude.Gameplay.NoteColors
open Prelude.Data.ScoreManager
open Interlude
open Interlude.Input
open Interlude.Input.Bind

type WindowType =
    | WINDOWED = 0
    | BORDERLESS = 1
    | FULLSCREEN = 2

type WindowResolution = 
    | Preset of index:int
    | Custom of width:int * height:int

type GameConfig = {
    WorkingDirectory: string
    Locale: string
    WindowMode: Setting<WindowType>
    Resolution: Setting<WindowResolution>
    FrameLimiter: Setting<float>
} with
    static member Default = {
        WorkingDirectory = ""
        Locale = "en_GB.txt"
        WindowMode = Setting(WindowType.BORDERLESS)
        Resolution = Setting(Custom (1024, 768))
        FrameLimiter = Setting(0.0)
    }

type Hotkeys = {
    Exit: Setting<Bind>
    Select: Setting<Bind>
    Search: Setting<Bind>
    Toolbar: Setting<Bind>
    Tooltip: Setting<Bind>
    Screenshot: Setting<Bind>
    Volume: Setting<Bind>
    Previous: Setting<Bind>
    Next: Setting<Bind>
    Start: Setting<Bind>
    End: Setting<Bind>

    Import: Setting<Bind>
    Options: Setting<Bind>
    Help: Setting<Bind>

    UpRate: Setting<Bind>
    DownRate: Setting<Bind>
    UpRateHalf: Setting<Bind>
    DownRateHalf: Setting<Bind>
    UpRateSmall: Setting<Bind>
    DownRateSmall: Setting<Bind>
} with
    static member Default = {
        Exit = Setting(mk Key.Escape)
        Select = Setting(mk Key.Enter)
        Search = Setting(mk Key.Tab)
        Tooltip = Setting(mk Key.Slash)
        Toolbar = Setting(ctrl Key.T)
        Screenshot = Setting(mk Key.F12)
        Volume = Setting(mk Key.AltLeft)
        Previous = Setting(mk Key.Left)
        Next = Setting(mk Key.Right)
        Start = Setting(mk Key.Home)
        End = Setting(mk Key.End)

        Import = Setting(ctrl Key.I)
        Options = Setting(ctrl Key.O)
        Help = Setting(ctrl Key.H)

        UpRate = Setting(mk Key.Plus)
        DownRate = Setting(mk Key.Minus)
        UpRateHalf = Setting(ctrl Key.Plus)
        DownRateHalf = Setting(ctrl Key.Minus)
        UpRateSmall = Setting(shift Key.Plus)
        DownRateSmall = Setting(shift Key.Minus)
    }

type ScoreSaving =
| Always = 0
| Pacemaker = 1
| PB = 2

type Pacemaker =
| Accuracy of float
| Lamp of Lamp

type FailType =
| Instant = 0
| AtEnd = 1
type ListSelection<'T> = int * 'T list

type ProfileStats = {
    //todo: rrd graph of improvement over time/session performances. or not, link to kamai and let kiam handle it
    TopPhysical: Setting<TopScore list>
    TopTechnical: Setting<TopScore list>
} with
    static member Default = {
        TopPhysical = Setting([])
        TopTechnical = Setting([])
    }

type GameOptions = {
    AudioOffset: NumSetting<float>
    AudioVolume: NumSetting<float>
    //CurrentProfile: Setting<string>
    CurrentChart: Setting<string>
    CurrentOptionsTab: Setting<string>
    EnabledThemes: List<string>

    ScrollSpeed: Setting<float>
    HitPosition: Setting<int>
    HitLighting: Setting<bool>
    Upscroll: Setting<bool>
    BackgroundDim: Setting<float>
    PerspectiveTilt: Setting<float>
    ScreenCoverUp: Setting<float>
    ScreenCoverDown: Setting<float>
    ScreenCoverFadeLength: Setting<int>
    ColorStyle: Setting<ColorConfig>
    KeymodePreference: Setting<int>
    UseKeymodePreference: Setting<bool>
    NoteSkin: Setting<string>

    Playstyles: Layout array
    HPSystems: Setting<ListSelection<HPSystemConfig>>
    AccSystems: Setting<ListSelection<AccuracySystemConfig>>
    ScoreSaveCondition: Setting<ScoreSaving>
    FailCondition: Setting<FailType>
    Pacemaker: Setting<Pacemaker>
    GameplayBinds: (Bind array) array

    Stats: ProfileStats

    ChartSortMode: Setting<string>
    ChartGroupMode: Setting<string>
    ChartColorMode: Setting<string>

    Hotkeys: Hotkeys
} with
    static member Default = {
        AudioOffset = FloatSetting(0.0, -500.0, 500.0)
        AudioVolume = FloatSetting(0.1, 0.0, 1.0)
        //CurrentProfile = Setting("")
        CurrentChart = Setting("")
        CurrentOptionsTab = Setting("General")
        EnabledThemes = new List<string>()

        ScrollSpeed = FloatSetting(2.05, 1.0, 3.0)
        HitPosition = IntSetting(0, -100, 400)
        HitLighting = Setting(false)
        Upscroll = Setting(false)
        BackgroundDim = FloatSetting(0.5, 0.0, 1.0)
        PerspectiveTilt = FloatSetting(0.0, -1.0, 1.0)
        ScreenCoverUp = FloatSetting(0.0, 0.0, 1.0)
        ScreenCoverDown = FloatSetting(0.0, 0.0, 1.0)
        ScreenCoverFadeLength = IntSetting(200, 0, 500)
        NoteSkin = Setting("default")
        ColorStyle = Setting(ColorConfig.Default)
        KeymodePreference = IntSetting(4, 3, 10)
        UseKeymodePreference = Setting(false)

        Playstyles = [|Layout.OneHand; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread|]
        HPSystems = Setting((0, [VG]))
        AccSystems = Setting((0, [SCPlus 4]))
        ScoreSaveCondition = Setting(ScoreSaving.Always)
        FailCondition = Setting(FailType.AtEnd)
        Pacemaker = Setting(Accuracy 0.95)

        //todo: move to scores database
        Stats = ProfileStats.Default

        ChartSortMode = Setting("Title")
        ChartGroupMode = Setting("Pack")
        ChartColorMode = Setting("Nothing")
        Hotkeys = Hotkeys.Default
        GameplayBinds = [|
            [|mk Key.Left; mk Key.Down; mk Key.Right|];
            [|mk Key.Z; mk Key.X; mk Key.Period; mk Key.Slash|];
            [|mk Key.Z; mk Key.X; mk Key.Space; mk Key.Period; mk Key.Slash|];
            [|mk Key.Z; mk Key.X; mk Key.C; mk Key.Comma; mk Key.Period; mk Key.Slash|];
            [|mk Key.Z; mk Key.X; mk Key.C; mk Key.Space; mk Key.Comma; mk Key.Period; mk Key.Slash|];
            [|mk Key.Z; mk Key.X; mk Key.C; mk Key.V; mk Key.Comma; mk Key.Period; mk Key.Slash; mk Key.RShift|];
            [|mk Key.Z; mk Key.X; mk Key.C; mk Key.V; mk Key.Space; mk Key.Comma; mk Key.Period; mk Key.Slash; mk Key.RShift|];
            [|mk Key.CapsLock; mk Key.Q; mk Key.W; mk Key.E; mk Key.V; mk Key.Space; mk Key.K; mk Key.L; mk Key.Semicolon; mk Key.Quote|]
        |]
    }

module Options =
    //forward ref for applying game config options. it is initialised in the constructor of Game
    let mutable applyOptions = fun () -> ()

    let resolutions: (struct (int * int)) array =
        [|struct (800, 600); struct (1024, 768); struct (1280, 800); struct (1280, 1024); struct (1366, 768); struct (1600, 900);
            struct (1600, 1024); struct (1680, 1050); struct (1920, 1080); struct (2715, 1527)|]

    let getResolution res =
        match res with
        | Custom (w, h) -> (true, struct (w, h))
        | Preset i ->
            let i = System.Math.Clamp(i, 0, Array.length resolutions - 1)
            (false, resolutions.[i])

    let mutable internal config = GameConfig.Default
    let mutable internal options = GameOptions.Default
    let internal configPath = Path.GetFullPath("config.json")

    let load() =
        try
            config <- JsonHelper.loadFile(configPath)
        with
        | :? FileNotFoundException -> Logging.Info("No config file found, creating it.") ""
        | err ->
            Logging.Critical("Could not load config.json! Maybe it is corrupt?") (err.ToString())
            Console.WriteLine("If you would like to launch anyway, press ENTER.")
            Console.WriteLine("If you would like to try and fix the problem youself, CLOSE THIS WINDOW.")
            Console.ReadLine() |> ignore
            Logging.Critical("User has chosen to launch game with default config.") ""
        
        if config.WorkingDirectory <> "" then Directory.SetCurrentDirectory(config.WorkingDirectory)
        Localisation.loadFile(config.Locale)
        
        try
            options <- JsonHelper.loadFile(Path.Combine(getDataPath("Data"), "options.json"))
        with
        | :? FileNotFoundException -> Logging.Info("No options file found, creating it.") ""
        | err ->
            Logging.Critical("Could not load options.json! Maybe it is corrupt?") (err.ToString())
            Console.WriteLine("If you would like to proceed anyway, press ENTER.")
            Console.WriteLine("If you would like to try and fix the problem youself, CLOSE THIS WINDOW.")
            Console.ReadLine() |> ignore
            Logging.Critical("User has chosen to proceed with game setup with default game settings.") ""

        Themes.loadThemes(options.EnabledThemes)
        Themes.changeNoteSkin(options.NoteSkin.Get())

    let save() =
        try
            JsonHelper.saveFile config configPath
            JsonHelper.saveFile options <| Path.Combine(getDataPath("Data"), "options.json")
        with
        | err -> Logging.Critical("Failed to write options/config to file.") (err.ToString())