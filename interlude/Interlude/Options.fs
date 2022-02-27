namespace Interlude

open System
open System.IO
open OpenTK.Windowing.GraphicsLibraryFramework
open Prelude.Common
open Prelude.Scoring
open Prelude.Gameplay.Layout
open Prelude.Data.Charts.Library.Imports
open Interlude
open Interlude.Input
open Interlude.Input.Bind

module Options =

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
        | ``30`` = 0
        | ``60`` = 1
        | ``120`` = 2
        | ``240`` = 3
        | ``480 (Recommended)`` = 4
        | Unlimited = 5
        | Vsync = 6

    type GameConfig = 
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
                FrameLimit = Setting.simple FrameLimit.``480 (Recommended)``
                Display = Setting.simple 0
                AudioDevice = Setting.simple 1
            }

    type Hotkeys =
        {
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
            Volume: Setting<Bind>

            Collections: Setting<Bind>
            AddToCollection: Setting<Bind>
            RemoveFromCollection: Setting<Bind>
            ReorderCollectionDown: Setting<Bind>
            ReorderCollectionUp: Setting<Bind>

            Mods: Setting<Bind>
            Autoplay: Setting<Bind>
            ChartInfo: Setting<Bind>

            Import: Setting<Bind>
            Options: Setting<Bind>
            Help: Setting<Bind>
            Tasks: Setting<Bind>
            Console: Setting<Bind>

            UpRate: Setting<Bind>
            DownRate: Setting<Bind>
            UpRateHalf: Setting<Bind>
            DownRateHalf: Setting<Bind>
            UpRateSmall: Setting<Bind>
            DownRateSmall: Setting<Bind>
        }
        static member Default = {
            Exit = Setting.simple(mk Keys.Escape)
            Select = Setting.simple(mk Keys.Enter)
            Search = Setting.simple(mk Keys.Tab)
            Toolbar = Setting.simple(ctrl Keys.T)
            Tooltip = Setting.simple(mk Keys.Slash)
            Delete = Setting.simple(mk Keys.Delete)
            Screenshot = Setting.simple(mk Keys.F12)
            Volume = Setting.simple(mk Keys.LeftAlt)
            Previous = Setting.simple(mk Keys.Left)
            Next = Setting.simple(mk Keys.Right)
            Up = Setting.simple(mk Keys.Up)
            Down = Setting.simple(mk Keys.Down)
            Start = Setting.simple(mk Keys.Home)
            End = Setting.simple(mk Keys.End)
            Skip = Setting.simple(mk Keys.Space)

            Collections = Setting.simple(mk Keys.N)
            AddToCollection = Setting.simple(mk Keys.RightBracket)
            RemoveFromCollection = Setting.simple(mk Keys.LeftBracket)
            ReorderCollectionDown = Setting.simple(ctrl Keys.RightBracket)
            ReorderCollectionUp = Setting.simple(ctrl Keys.LeftBracket)

            Mods = Setting.simple(mk Keys.M)
            Autoplay = Setting.simple(ctrl Keys.A)
            ChartInfo = Setting.simple(mk Keys.Period)

            Import = Setting.simple(ctrl Keys.I)
            Options = Setting.simple(ctrl Keys.O)
            Help = Setting.simple(ctrl Keys.H)
            Tasks = Setting.simple(mk Keys.F8)
            Console = Setting.simple(mk Keys.GraveAccent)

            UpRate = Setting.simple(mk Keys.Equal)
            DownRate = Setting.simple(mk Keys.Minus)
            UpRateHalf = Setting.simple(ctrl Keys.Equal)
            DownRateHalf = Setting.simple(ctrl Keys.Minus)
            UpRateSmall = Setting.simple(shift Keys.Equal)
            DownRateSmall = Setting.simple(shift Keys.Minus)
        }

    type Keymode =
        | ``3K`` = 3
        | ``4K`` = 4
        | ``5K`` = 5
        | ``6K`` = 6
        | ``7K`` = 7
        | ``8K`` = 8
        | ``9K`` = 9
        | ``10K`` = 10

    type ScoreSaving =
        | Always = 0
        | Pacemaker = 1
        | PersonalBest = 2

    type Pacemaker =
        | Accuracy of float
        | Lamp of int

    type FailType =
        | Instant = 0
        | EndOfSong = 1

    type WatcherSelection<'T> = 'T list
    module WatcherSelection =
        let cycleForward xs =
            match xs with
            | x :: xs -> xs @ [x]
            | _ -> failwith "impossible"

        let rec cycleBackward xs =
            match xs with
            | [] -> failwith "impossible"
            | x :: [] -> [x]
            | x :: xs -> match cycleBackward xs with (y :: ys) -> (y :: x :: ys) | _ -> failwith "impossible by case 2"

        let contains x xs = xs |> List.exists (fun o -> o = x)

        let delete x xs = xs |> List.filter (fun o -> o <> x)

        let add x xs = x :: xs

    type ScreenCoverOptions =
        {
            Enabled: Setting<bool>
            Sudden: Setting.Bounded<float>
            Hidden: Setting.Bounded<float>
            FadeLength: Setting.Bounded<int>
            Color: Setting<Color>
        }

    type GameOptions =
        {
            VisualOffset: Setting.Bounded<float>
            AudioOffset: Setting.Bounded<float>
            AudioVolume: Setting.Bounded<float>
            CurrentChart: Setting<string>
            Theme: Setting<string>

            ScrollSpeed: Setting.Bounded<float>
            HitPosition: Setting.Bounded<int>
            HitLighting: Setting<bool>
            Upscroll: Setting<bool>
            BackgroundDim: Setting.Bounded<float>
            PerspectiveTilt: Setting.Bounded<float>
            ScreenCover: ScreenCoverOptions
            KeymodePreference: Setting<Keymode>
            UseKeymodePreference: Setting<bool>
            Noteskin: Setting<string>

            Playstyles: Layout array
            Rulesets: Setting<WatcherSelection<string>>
            ScoreSaveCondition: Setting<ScoreSaving>
            FailCondition: Setting<FailType>
            Pacemaker: Setting<Pacemaker>

            OsuMount: Setting<MountedChartSource option>
            StepmaniaMount: Setting<MountedChartSource option>
            EtternaMount: Setting<MountedChartSource option>

            ChartSortMode: Setting<string>
            ChartGroupMode: Setting<string>
            //ChartColorMode: Setting<string>
            ScoreSortMode: Setting<int>
            GameplayBinds: (Bind array) array

            EnableConsole: Setting<bool>
            Hotkeys: Hotkeys
        }
        static member Default = {
            VisualOffset = Setting.bounded 0.0 -500.0 500.0 |> Setting.round 0
            AudioOffset = Setting.bounded 0.0 -500.0 500.0 |> Setting.round 0
            AudioVolume = Setting.percent 0.05
            CurrentChart = Setting.simple ""
            Theme = Setting.simple "*default"

            ScrollSpeed = Setting.bounded 2.05 1.0 5.0 |> Setting.round 2
            HitPosition = Setting.bounded 0 -300 600
            HitLighting = Setting.simple false
            Upscroll = Setting.simple false
            BackgroundDim = Setting.percent 0.5
            PerspectiveTilt = Setting.bounded 0.0 -1.0 1.0 |> Setting.round 2
            ScreenCover = 
                { 
                    Enabled = Setting.simple false
                    Sudden = Setting.percent 0.0
                    Hidden = Setting.percent 0.45
                    FadeLength = Setting.bounded 200 0 500
                    Color = Setting.simple Color.Black
                }
            Noteskin = Setting.simple "*defaultBar.isk"
            KeymodePreference = Setting.simple Keymode.``4K``
            UseKeymodePreference = Setting.simple false

            Playstyles = [|Layout.OneHand; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread|]
            Rulesets =
                Setting.simple [Content.Rulesets.DEFAULT]
                |> Setting.map
                    id
                    ( fun xs -> 
                        let filtered = 
                            List.filter 
                                ( fun x -> 
                                    if Content.Rulesets.exists x then true
                                    else Logging.Debug(sprintf "Score system '%s' not found, deselecting" x); false
                                ) xs
                        let l = if filtered.IsEmpty then [Content.Rulesets.DEFAULT] else filtered
                        Content.Rulesets.switch (List.head l) false
                        l
                    )
            ScoreSaveCondition = Setting.simple ScoreSaving.Always
            FailCondition = Setting.simple FailType.EndOfSong
            Pacemaker = Setting.simple (Accuracy 0.95)

            OsuMount = Setting.simple None
            StepmaniaMount = Setting.simple None
            EtternaMount = Setting.simple None

            ChartSortMode = Setting.simple "Title"
            ChartGroupMode = Setting.simple "Pack"
            EnableConsole = Setting.simple false
            ScoreSortMode = Setting.simple 0
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
    let mutable applyOptions: unit -> unit = ignore

    let mutable internal config = GameConfig.Default
    let mutable internal options = GameOptions.Default
    let internal configPath = Path.GetFullPath "config.json"
    let firstLaunch = not (File.Exists configPath)

    let load() =
        config <- loadImportantJsonFile "Config" configPath config true
        Localisation.loadFile config.Locale
        if config.WorkingDirectory <> "" then Directory.SetCurrentDirectory config.WorkingDirectory
        options <- loadImportantJsonFile "Options" (Path.Combine(getDataPath "Data", "options.json")) options true

    let save() =
        try
            JSON.ToFile(configPath, true) config
            JSON.ToFile(Path.Combine(getDataPath "Data", "options.json"), true) options
        with err -> Logging.Critical("Failed to write options/config to file.", err)

    let getCurrentRuleset() = Content.Rulesets.current