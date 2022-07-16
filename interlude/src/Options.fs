namespace Interlude

open System.IO
open System.Collections.Generic
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Json
open Percyqaz.Flux.Input
open Percyqaz.Flux.Input.Bind
open Prelude.Common
open Prelude.Gameplay.Layout
open Prelude.Data.Charts.Library.Imports
open Interlude

module Options =

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

    [<Json.AutoCodec>]
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

    [<Json.AutoCodec>]
    type ScreenCoverOptions =
        {
            Enabled: Setting<bool>
            Sudden: Setting.Bounded<float>
            Hidden: Setting.Bounded<float>
            FadeLength: Setting.Bounded<int>
            Color: Setting<Color>
        }

    [<Json.AutoCodec(false)>]
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

    let mutable internal config = Percyqaz.Flux.Windowing.Config.Default

    do 
        // Register decoding rules for Percyqaz.Flux config
        JSON.WithAutoCodec<Percyqaz.Flux.Windowing.Config>(false)
            .WithAutoCodec<Percyqaz.Flux.Windowing.WindowResolution>()
            .WithAutoCodec<Percyqaz.Flux.Input.Bind>() |> ignore

    let mutable options = GameOptions.Default

    module Hotkeys =

        let init(d: Dictionary<Hotkey, Bind>) =
            Hotkeys.register "search" (mk Keys.Tab)
            Hotkeys.register "toolbar" (ctrl Keys.T)
            Hotkeys.register "tooltip" (mk Keys.Slash)
            Hotkeys.register "delete" (mk Keys.Delete)
            Hotkeys.register "screenshot" (mk Keys.F12)
            Hotkeys.register "volume" (mk Keys.LeftAlt)
            Hotkeys.register "previous" (mk Keys.Left)
            Hotkeys.register "next" (mk Keys.Right)
            Hotkeys.register "previous_group" (mk Keys.PageUp)
            Hotkeys.register "next_group" (mk Keys.PageDown)
            Hotkeys.register "start" (mk Keys.Home)
            Hotkeys.register "end" (mk Keys.End)

            Hotkeys.register "collections" (mk Keys.N)
            Hotkeys.register "add_to_collection" (mk Keys.RightBracket)
            Hotkeys.register "remove_from_collection" (mk Keys.LeftBracket)
            Hotkeys.register "move_down_in_collection" (ctrl Keys.RightBracket)
            Hotkeys.register "move_up_in_collection" (ctrl Keys.LeftBracket)
            Hotkeys.register "sort_mode" (mk Keys.Comma)
            Hotkeys.register "group_mode" (mk Keys.Period)

            Hotkeys.register "mods" (mk Keys.M)
            Hotkeys.register "autoplay" (ctrl Keys.A)

            Hotkeys.register "import" (ctrl Keys.I)
            Hotkeys.register "options" (ctrl Keys.O)
            Hotkeys.register "help" (ctrl Keys.H)
            Hotkeys.register "tasks" (mk Keys.F8)
            Hotkeys.register "console" (mk Keys.GraveAccent)

            Hotkeys.register "uprate" (mk Keys.Equal)
            Hotkeys.register "downrate" (mk Keys.Minus)
            Hotkeys.register "uprate_half" (ctrl Keys.Equal)
            Hotkeys.register "downrate_half" (ctrl Keys.Minus)
            Hotkeys.register "uprate_small" (shift Keys.Equal)
            Hotkeys.register "downrate_small" (shift Keys.Minus)

            Hotkeys.register "skip" (mk Keys.Space)
            Hotkeys.register "retry" (ctrl Keys.R)

            Hotkeys.import d

    let private configPath = Path.GetFullPath "config.json"
    let firstLaunch = not (File.Exists configPath)

    let load() =
        config <- loadImportantJsonFile "Config" configPath true
        Localisation.loadFile config.Locale
        if config.WorkingDirectory <> "" then Directory.SetCurrentDirectory config.WorkingDirectory
        options <- loadImportantJsonFile "Options" (Path.Combine(getDataPath "Data", "options.json")) true

    let save() =
        Hotkeys.export options.Hotkeys
        try
            JSON.ToFile(configPath, true) config
            JSON.ToFile(Path.Combine(getDataPath "Data", "options.json"), true) options
        with err -> Logging.Critical("Failed to write options/config to file.", err)

    let getCurrentRuleset() = Content.Rulesets.current