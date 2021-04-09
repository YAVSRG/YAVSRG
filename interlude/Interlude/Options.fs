namespace Interlude

open System
open System.Collections.Generic
open System.IO
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Percyqaz.Json
open Prelude.Gameplay.Score
open Prelude.Gameplay.Layout
open Prelude.Gameplay.NoteColors
open Prelude.Data.ScoreManager
open Interlude
open Interlude.Input
open Interlude.Input.Bind

module Options =

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
        Previous: Setting<Bind>
        Next: Setting<Bind>
        Up: Setting<Bind>
        Down: Setting<Bind>
        Start: Setting<Bind>
        End: Setting<Bind>

        Skip: Setting<Bind>
        Search: Setting<Bind>
        Toolbar: Setting<Bind>
        Tooltip: Setting<Bind>
        Delete: Setting<Bind>
        Screenshot: Setting<Bind>
        Tasklist: Setting<Bind>
        Volume: Setting<Bind>

        Mods: Setting<Bind>
        Scoreboard: Setting<Bind>
        ChartInfo: Setting<Bind>

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
            Exit = Setting(mk Keys.Escape)
            Select = Setting(mk Keys.Enter)
            Search = Setting(mk Keys.Tab)
            Toolbar = Setting(ctrl Keys.T)
            Tooltip = Setting(mk Keys.Slash)
            Delete = Setting(mk Keys.Delete)
            Screenshot = Setting(mk Keys.F12)
            Tasklist = Setting(mk Keys.F8)
            Volume = Setting(mk Keys.LeftAlt)
            Previous = Setting(mk Keys.Left)
            Next = Setting(mk Keys.Right)
            Up = Setting(mk Keys.Up)
            Down = Setting(mk Keys.Down)
            Start = Setting(mk Keys.Home)
            End = Setting(mk Keys.End)
            Skip = Setting(mk Keys.Space)

            Mods = Setting(mk Keys.M)
            Scoreboard = Setting(mk Keys.Comma)
            ChartInfo = Setting(mk Keys.Period)

            Import = Setting(ctrl Keys.I)
            Options = Setting(ctrl Keys.O)
            Help = Setting(ctrl Keys.H)

            UpRate = Setting(mk Keys.Equal)
            DownRate = Setting(mk Keys.Minus)
            UpRateHalf = Setting(ctrl Keys.Equal)
            DownRateHalf = Setting(ctrl Keys.Minus)
            UpRateSmall = Setting(shift Keys.Equal)
            DownRateSmall = Setting(shift Keys.Minus)
        }

    type ScoreSaving =
    | Always = 0
    | Pacemaker = 1
    | PersonalBest = 2

    type Pacemaker =
    | Accuracy of float
    | Lamp of Lamp

    type FailType =
    | Instant = 0
    | EndOfSong = 1

    type WatcherSelection<'T> = 'T * 'T list
    module WatcherSelection =
        let cycleForward (main, alts) =
            match alts with
            | [] -> (main, alts)
            | x :: xs -> (x, xs @ [main])

        let rec cycleBackward (main, alts) =
            match alts with
            | [] -> (main, alts)
            | x :: xs -> let (m, a) = cycleBackward (x, xs) in (m, main :: a)

    type ProfileStats = {
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
        CurrentChart: Setting<string>
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
        HPSystems: Setting<WatcherSelection<HPSystemConfig>>
        AccSystems: Setting<WatcherSelection<AccuracySystemConfig>>
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
            CurrentChart = Setting("")
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
            HPSystems = Setting(VG, [])
            AccSystems = Setting(SCPlus (4, false), [])
            ScoreSaveCondition = Setting(ScoreSaving.Always)
            FailCondition = Setting(FailType.EndOfSong)
            Pacemaker = Setting(Accuracy 0.95)

            //todo: move to scores database
            Stats = ProfileStats.Default

            ChartSortMode = Setting("Title")
            ChartGroupMode = Setting("Pack")
            ChartColorMode = Setting("Nothing")
            Hotkeys = Hotkeys.Default
            GameplayBinds = [|
                [|mk Keys.Left; mk Keys.Down; mk Keys.Right|];
                [|mk Keys.Z; mk Keys.X; mk Keys.Period; mk Keys.Slash|];
                [|mk Keys.Z; mk Keys.X; mk Keys.Space; mk Keys.Period; mk Keys.Slash|];
                [|mk Keys.Z; mk Keys.X; mk Keys.C; mk Keys.Comma; mk Keys.Period; mk Keys.Slash|];
                [|mk Keys.Z; mk Keys.X; mk Keys.C; mk Keys.Space; mk Keys.Comma; mk Keys.Period; mk Keys.Slash|];
                [|mk Keys.Z; mk Keys.X; mk Keys.C; mk Keys.V; mk Keys.Comma; mk Keys.Period; mk Keys.Slash; mk Keys.RightShift|];
                [|mk Keys.Z; mk Keys.X; mk Keys.C; mk Keys.V; mk Keys.Space; mk Keys.Comma; mk Keys.Period; mk Keys.Slash; mk Keys.RightShift|];
                [|mk Keys.CapsLock; mk Keys.Q; mk Keys.W; mk Keys.E; mk Keys.V; mk Keys.Space; mk Keys.K; mk Keys.L; mk Keys.Semicolon; mk Keys.Apostrophe|]
            |]
        }

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
        config <- loadImportantJsonFile("Config")(configPath)(config)(true)
        if config.WorkingDirectory <> "" then Directory.SetCurrentDirectory(config.WorkingDirectory)
        Localisation.loadFile(config.Locale)
        options <- loadImportantJsonFile("Options")(Path.Combine(getDataPath("Data"), "options.json"))(options)(true)

        Themes.refreshAvailableThemes()
        Themes.loadThemes(options.EnabledThemes)
        Themes.changeNoteSkin(options.NoteSkin.Get())

    let save() =
        try
            Json.toFile(configPath, true) config
            Json.toFile(Path.Combine(getDataPath("Data"), "options.json"), true) options
        with err -> Logging.Critical("Failed to write options/config to file.") (err.ToString())

    let loadImportantJsonFile<'T> name path (defaultData: 'T) prompt =
        if File.Exists(path) then
            let p = Path.ChangeExtension(path, ".bak")
            if File.Exists(p) then File.Copy(p, Path.ChangeExtension(path, ".bak2"), true)
            File.Copy(path, p, true)
            try
                Json.fromFile(path) |> JsonResult.value
            with err ->
                Logging.Critical(sprintf "Could not load %s! Maybe it is corrupt?" <| Path.GetFileName(path)) (err.ToString())
                if prompt then
                    Console.WriteLine("If you would like to launch anyway, press ENTER.")
                    Console.WriteLine("If you would like to try and fix the problem youself, CLOSE THIS WINDOW.")
                    Console.ReadLine() |> ignore
                    Logging.Critical("User has chosen to launch game with default data.") ""
                defaultData
        else
            Logging.Info(sprintf "No %s file found, creating it." name) ""
            defaultData