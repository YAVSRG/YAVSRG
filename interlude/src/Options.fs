namespace Interlude

open System.IO
open System.Collections.Generic
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Prelude.Common
open Prelude.Gameplay.Layout
open Prelude.Data.Charts.Library.Imports
open Interlude
open Interlude.Input
open Interlude.Input.Bind

module Options =

    (*
        Core game config
        This stuff is always stored with the .exe, not the game data folder
    *)

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

    (*
        User settings
    *)

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

    // todo: change name, this is from the old codebase
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

    type Hotkey =

        | NONE = -1

        // All-purpose/menus
        | Exit = 0
        | Select = 1
        | Previous = 2
        | Next = 3 
        | PreviousGroup = 4
        | NextGroup = 5
        | Up = 6
        | Down = 7
        | Start = 8
        | End = 9
        | Search = 10
        | Toolbar = 11
        | Tooltip = 12
        | Delete = 13
        | Screenshot = 14
        | Volume = 15
        
        // Shortcuts
        | Import = 100
        | Options = 101
        | Help = 102
        | Tasks = 103
        | Console = 104
        
        // Level select
        | UpRate = 200
        | UpRateHalf = 201
        | UpRateSmall = 202
        | DownRate = 203
        | DownRateHalf = 204
        | DownRateSmall = 205
        | Collections = 206
        | AddToCollection = 207
        | RemoveFromCollection = 208
        | MoveDownInCollection = 209
        | MoveUpInCollection = 210
        | Mods = 211
        | Autoplay = 212
        | SortMode = 213
        | GroupMode = 214
        
        // Gameplay
        | Skip = 300
        | Retry = 301

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
            ChartSortReverse: Setting<bool>
            ChartGroupMode: Setting<string>
            ChartGroupReverse: Setting<bool>
            ScoreSortMode: Setting<int>

            SelectedCollection: Setting<string>
            SelectedTable: Setting<string>
            GameplayBinds: (Bind array) array

            EnableConsole: Setting<bool>
            Hotkeys: Dictionary<Hotkey, Bind>
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
                        if Content.first_init then xs else
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
            ChartSortReverse = Setting.simple false
            ChartGroupMode = Setting.simple "Pack"
            ChartGroupReverse = Setting.simple false
            ScoreSortMode = Setting.simple 0

            SelectedCollection = Setting.simple ""
            SelectedTable = Setting.simple ""

            EnableConsole = Setting.simple false
            Hotkeys = Dictionary<Hotkey, Bind>()
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

    // forward ref for applying game config options. it is initialised in the constructor of Game
    let mutable applyOptions: unit -> unit = ignore

    let mutable internal config = GameConfig.Default
    let mutable options = GameOptions.Default

    module Hotkeys =

        let defaultHotkeys = 
            Map.ofList [
                Hotkey.NONE, Dummy

                Hotkey.Exit, mk Keys.Escape
                Hotkey.Select, mk Keys.Enter
                Hotkey.Search, mk Keys.Tab
                Hotkey.Toolbar, ctrl Keys.T
                Hotkey.Tooltip, mk Keys.Slash
                Hotkey.Delete, mk Keys.Delete
                Hotkey.Screenshot, mk Keys.F12
                Hotkey.Volume, mk Keys.LeftAlt
                Hotkey.Previous, mk Keys.Left
                Hotkey.Next, mk Keys.Right
                Hotkey.PreviousGroup, mk Keys.PageUp
                Hotkey.NextGroup, mk Keys.PageDown
                Hotkey.Up, mk Keys.Up
                Hotkey.Down, mk Keys.Down
                Hotkey.Start, mk Keys.Home
                Hotkey.End, mk Keys.End

                Hotkey.Collections, mk Keys.N
                Hotkey.AddToCollection, mk Keys.RightBracket
                Hotkey.RemoveFromCollection, mk Keys.LeftBracket
                Hotkey.MoveDownInCollection, ctrl Keys.RightBracket
                Hotkey.MoveUpInCollection, ctrl Keys.LeftBracket
                Hotkey.SortMode, mk Keys.Comma
                Hotkey.GroupMode, mk Keys.Period

                Hotkey.Mods, mk Keys.M
                Hotkey.Autoplay, ctrl Keys.A

                Hotkey.Import, ctrl Keys.I
                Hotkey.Options, ctrl Keys.O
                Hotkey.Help, ctrl Keys.H
                Hotkey.Tasks, mk Keys.F8
                Hotkey.Console, mk Keys.GraveAccent

                Hotkey.UpRate, mk Keys.Equal
                Hotkey.DownRate, mk Keys.Minus
                Hotkey.UpRateHalf, ctrl Keys.Equal
                Hotkey.DownRateHalf, ctrl Keys.Minus
                Hotkey.UpRateSmall, shift Keys.Equal
                Hotkey.DownRateSmall, shift Keys.Minus

                Hotkey.Skip, mk Keys.Space
                Hotkey.Retry, ctrl Keys.R
            ]

        let debug() =
            for v in System.Enum.GetValues typeof<Hotkey> do
                if not(defaultHotkeys.ContainsKey (v :?> Hotkey)) then
                    failwithf "Missing a default bind for: %A" v

        let init(d: Dictionary<Hotkey, Bind>) =
            for (key, value) in defaultHotkeys |> Map.toSeq do
                if not (d.ContainsKey key) || key = Hotkey.NONE then
                    d.[key] <- value

    let (!|) (hotkey: Hotkey) = options.Hotkeys.[hotkey]

    let private configPath = Path.GetFullPath "config.json"
    let firstLaunch = not (File.Exists configPath)

    let load() =
        config <- loadImportantJsonFile "Config" configPath config true
        Localisation.loadFile config.Locale
        if config.WorkingDirectory <> "" then Directory.SetCurrentDirectory config.WorkingDirectory
        options <- loadImportantJsonFile "Options" (Path.Combine(getDataPath "Data", "options.json")) options true
        Hotkeys.init(options.Hotkeys)
        Hotkeys.debug()

    let save() =
        try
            JSON.ToFile(configPath, true) config
            JSON.ToFile(Path.Combine(getDataPath "Data", "options.json"), true) options
        with err -> Logging.Critical("Failed to write options/config to file.", err)

    let getCurrentRuleset() = Content.Rulesets.current