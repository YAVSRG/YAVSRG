namespace Interlude.Options

open System.Collections.Generic
open System.IO
open OpenTK.Input
open Prelude.Common
open Prelude.Json
open Prelude.Data.Profiles
open Prelude.Gameplay.Score
open Interlude
open Interlude.Input

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
    WindowMode: WindowType
    Resolution: WindowResolution
    FrameLimiter: float
} with
    static member Default = {
        WorkingDirectory = ""
        Locale = "en_GB.txt"
        WindowMode = WindowType.BORDERLESS
        Resolution = Custom (1024, 768)
        FrameLimiter = 0.0
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
        Exit = Setting(Key Key.Escape)
        Select = Setting(Key Key.Enter)
        Search = Setting(Key Key.Tab)
        Tooltip = Setting(Key Key.Slash)
        Toolbar = Setting(Key Key.T |> Ctrl)
        Screenshot = Setting(Key Key.F12)
        Volume = Setting(Key Key.AltLeft)
        Previous = Setting(Key Key.Left)
        Next = Setting(Key Key.Right)
        Start = Setting(Key Key.Home)
        End = Setting(Key Key.End)

        Import = Setting(Key Key.I |> Ctrl)
        Options = Setting(Key Key.O |> Ctrl)
        Help = Setting(Key Key.H |> Ctrl)

        UpRate = Setting(Key Key.Plus)
        DownRate = Setting(Key Key.Minus)
        UpRateHalf = Setting(Key Key.Plus |> Ctrl)
        DownRateHalf = Setting(Key Key.Minus |> Ctrl)
        UpRateSmall = Setting(Key Key.Plus |> Shift)
        DownRateSmall = Setting(Key Key.Minus |> Shift)
    }

type GameOptions = {
    AudioOffset: NumSetting<float>
    AudioVolume: NumSetting<float>
    CurrentProfile: Setting<string>
    CurrentChart: Setting<string>
    CurrentOptionsTab: Setting<string>
    EnabledThemes: List<string>
    AccuracySystems: List<AccuracySystemConfig>
    HPSystems: List<HPSystemConfig>
    Hotkeys: Hotkeys
    GameplayBinds: (Bind array) array
} with
    static member Default = {
        AudioOffset = FloatSetting(0.0, -500.0, 500.0)
        AudioVolume = FloatSetting(0.1, 0.0, 1.0)
        CurrentProfile = Setting("")
        CurrentChart = Setting("")
        CurrentOptionsTab = Setting("General")
        EnabledThemes = new List<string>()
        AccuracySystems = new List<AccuracySystemConfig>()
        HPSystems = new List<HPSystemConfig>()
        Hotkeys = Hotkeys.Default
        GameplayBinds = [|
            [|Key Key.Left; Key Key.Down; Key Key.Right|];
            [|Key Key.Z; Key Key.X; Key Key.Period; Key Key.Slash|];
            [|Key Key.Z; Key Key.X; Key Key.Space; Key Key.Period; Key Key.Slash|];
            [|Key Key.Z; Key Key.X; Key Key.C; Key Key.Comma; Key Key.Period; Key Key.Slash|];
            [|Key Key.Z; Key Key.X; Key Key.C; Key Key.Space; Key Key.Comma; Key Key.Period; Key Key.Slash|];
            [|Key Key.Z; Key Key.X; Key Key.C; Key Key.V; Key Key.Comma; Key Key.Period; Key Key.Slash; Key Key.RShift|];
            [|Key Key.Z; Key Key.X; Key Key.C; Key Key.V; Key Key.Space; Key Key.Comma; Key Key.Period; Key Key.Slash; Key Key.RShift|];
            [|Key Key.CapsLock; Key Key.Q; Key Key.W; Key Key.E; Key Key.V; Key Key.Space; Key Key.K; Key Key.L; Key Key.Semicolon; Key Key.Quote|]|]
    }

module Options =
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
    let mutable internal profile = Profile.Default
    let internal configPath = Path.GetFullPath("config.json")
    let profiles = new List<Profile>()

    let load() =
        try
            config <- JsonHelper.loadFile(configPath)
        with
        | :? FileNotFoundException -> Logging.Info("No config file found, creating it.") ""
        | err ->
            Logging.Critical("Could not load config.json! Maybe it is corrupt?") (err.ToString())
            System.Console.WriteLine("If you would like to launch anyway, press ENTER.")
            System.Console.WriteLine("If you would like to try and fix the problem youself, CLOSE THIS WINDOW.")
            System.Console.ReadLine() |> ignore
            Logging.Critical("User has chosen to launch game with default config.") ""
        
        if config.WorkingDirectory <> "" then Directory.SetCurrentDirectory(config.WorkingDirectory)
        Localisation.loadFile(config.Locale)
        
        try
            options <- JsonHelper.loadFile(Path.Combine(getDataPath("Data"), "options.json"))
        with
        | :? FileNotFoundException -> Logging.Info("No options file found, creating it.") ""
        | err ->
            Logging.Critical("Could not load options.json! Maybe it is corrupt?") (err.ToString())
            System.Console.WriteLine("If you would like to proceed anyway, press ENTER.")
            System.Console.WriteLine("If you would like to try and fix the problem youself, CLOSE THIS WINDOW.")
            System.Console.ReadLine() |> ignore
            Logging.Critical("User has chosen to proceed with game setup with default game settings.") ""

        for pf in Directory.EnumerateFiles(Profile.profilePath) do
            try
                let data = Profile.load pf
                Profile.save data
                profiles.Add(data)
                if data.UUID = options.CurrentProfile.Get() then profile <- data
            with
            | err -> Logging.Error("Failed to load profile: " + Path.GetFileName(pf)) (err.ToString())
        options.CurrentProfile.Set(profile.UUID)
        Themes.loadThemes(options.EnabledThemes)
        Themes.changeNoteSkin(profile.NoteSkin.Get())
        Logging.Debug(sprintf "Current profile: %s (%s)" (profile.Name.Get()) profile.UUID) ""

    let changeProfile(uuid: string) =
        Profile.save profile
        match Seq.tryFind (fun p -> p.UUID = uuid) profiles with
        | Some p ->
            options.CurrentProfile.Set(p.UUID)
            profile <- p
        | None -> Logging.Error("No profile with UUID '" + uuid + "' exists") ""

    let save() =
        try
            JsonHelper.saveFile config configPath
            JsonHelper.saveFile options <| Path.Combine(getDataPath("Data"), "options.json")
            Profile.save profile
        with
        | err -> Logging.Critical("Failed to write options/config to file.") (err.ToString())