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
open Prelude.Data.Charts.Library
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

    [<Json.AutoCodec>]
    [<RequireQualifiedAccess>]
    type Pacemaker =
        | Accuracy of float
        | Lamp of int
        static member Default = Accuracy 0.95

    [<RequireQualifiedAccess>]
    type ScoreGraphMode =
        | HP = 0
        | None = 0
        | Combo = 1

    type FailType =
        | Instant = 0
        | EndOfSong = 1

    [<Json.AutoCodec>]
    type LaneCoverOptions =
        {
            Enabled: Setting<bool>
            Sudden: Setting.Bounded<float32>
            Hidden: Setting.Bounded<float32>
            FadeLength: Setting.Bounded<float32>
            Color: Setting<Color>
        }
        member this.LoadPreset(p: LaneCoverOptions) =
            this.Enabled.Value <- p.Enabled.Value
            this.Sudden.Value <- p.Sudden.Value
            this.Hidden.Value <- p.Hidden.Value
            this.FadeLength.Value <- p.FadeLength.Value
            this.Color.Value <- p.Color.Value

    [<Json.AutoCodec>]
    type Preset =
        {
            Name: string
            Locked: bool

            VisualOffset: float32
            ScrollSpeed: float32
            HitPosition: float32
            Upscroll: bool
            LaneCover: LaneCoverOptions
            Noteskin: string
        }

    [<Json.AutoCodec(false)>]
    type GameOptions =
        {
            VisualOffset: Setting.Bounded<float32>
            AudioOffset: Setting.Bounded<float32>
            AudioVolume: Setting.Bounded<float>
            CurrentChart: Setting<string>
            Theme: Setting<string>

            ScrollSpeed: Setting.Bounded<float32>
            HitPosition: Setting.Bounded<float32>
            HitLighting: Setting<bool>
            Upscroll: Setting<bool>
            BackgroundDim: Setting.Bounded<float32>
            LaneCover: LaneCoverOptions
            KeymodePreference: Setting<Keymode>
            UseKeymodePreference: Setting<bool>
            Noteskin: Setting<string>

            Playstyles: Layout array
            SelectedRuleset: Setting<string>
            FailCondition: Setting<FailType>
            Pacemakers: Dictionary<string, Pacemaker>
            SaveScoreIfUnderPace: Setting<bool>

            OsuMount: Setting<MountedChartSource option>
            StepmaniaMount: Setting<MountedChartSource option>
            EtternaMount: Setting<MountedChartSource option>

            ChartSortMode: Setting<string>
            ChartSortReverse: Setting<bool>
            ChartGroupMode: Setting<string>
            LibraryMode: Setting<LibraryMode>
            ChartGroupReverse: Setting<bool>
            ScoreSortMode: Setting<int>

            SelectedCollection: Setting<string option>
            Table: Setting<string option>
            GameplayBinds: (Bind array) array

            EnableConsole: Setting<bool>
            Hotkeys: Dictionary<Hotkey, Bind>

            Preset1: Setting<Preset option>
            Preset2: Setting<Preset option>
            Preset3: Setting<Preset option>

            VanishingNotes: Setting<bool>
            AutoCalibrateOffset: Setting<bool>
            AdvancedRecommendations: Setting<bool>
            ScoreGraphMode: Setting<ScoreGraphMode>
        }
        static member Default = {
            VisualOffset = Setting.bounded 0.0f -500.0f 500.0f |> Setting.roundf 0
            AudioOffset = Setting.bounded 0.0f -500.0f 500.0f |> Setting.roundf 0
            AudioVolume = Setting.percent 0.05
            CurrentChart = Setting.simple ""
            Theme = Setting.simple "*default"

            ScrollSpeed = Setting.bounded 2.05f 1.0f 5.0f |> Setting.roundf 2
            HitPosition = Setting.bounded 0.0f -300.0f 600.0f
            HitLighting = Setting.simple false
            Upscroll = Setting.simple false
            BackgroundDim = Setting.percentf 0.5f
            LaneCover = 
                { 
                    Enabled = Setting.simple false
                    Sudden = Setting.percentf 0.0f
                    Hidden = Setting.percentf 0.45f
                    FadeLength = Setting.bounded 200f 0f 500f
                    Color = Setting.simple Color.Black
                }
            Noteskin = Setting.simple "*defaultBar.isk"
            KeymodePreference = Setting.simple Keymode.``4K``
            UseKeymodePreference = Setting.simple false

            // playstyles are hints to the difficulty calc on how the hands are positioned
            // will be removed when difficulty calc gets scrapped
            // there is no way to edit these
            Playstyles = [|Layout.OneHand; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread; Layout.LeftOne; Layout.Spread|]
            SelectedRuleset = 
                Setting.simple Content.Rulesets.DEFAULT_ID
                |> Setting.trigger (fun t -> 
                    if Content.first_init then Percyqaz.Flux.UI.Root.sync(fun () -> Content.Rulesets.switch t)
                    else Content.Rulesets.switch t
                )
            FailCondition = Setting.simple FailType.EndOfSong
            Pacemakers = Dictionary<string, Pacemaker>()
            SaveScoreIfUnderPace = Setting.simple true

            OsuMount = Setting.simple None
            StepmaniaMount = Setting.simple None
            EtternaMount = Setting.simple None

            ChartSortMode = Setting.simple "Title"
            ChartSortReverse = Setting.simple false
            ChartGroupMode = Setting.simple "Pack"
            LibraryMode = Setting.simple LibraryMode.All
            ChartGroupReverse = Setting.simple false
            ScoreSortMode = Setting.simple 0
            
            SelectedCollection = Setting.simple None
            Table = Setting.simple None

            EnableConsole = Setting.simple false
            Hotkeys = Dictionary<Hotkey, Bind>()
            GameplayBinds = [|
                [|mk Keys.Left; mk Keys.Down; mk Keys.Right|]
                [|mk Keys.Z; mk Keys.X; mk Keys.Period; mk Keys.Slash|]
                [|mk Keys.Z; mk Keys.X; mk Keys.Space; mk Keys.Period; mk Keys.Slash|]
                [|mk Keys.Z; mk Keys.X; mk Keys.C; mk Keys.Comma; mk Keys.Period; mk Keys.Slash|]
                [|mk Keys.Z; mk Keys.X; mk Keys.C; mk Keys.Space; mk Keys.Comma; mk Keys.Period; mk Keys.Slash|]
                [|mk Keys.Z; mk Keys.X; mk Keys.C; mk Keys.V; mk Keys.Comma; mk Keys.Period; mk Keys.Slash; mk Keys.RightShift|]
                [|mk Keys.Z; mk Keys.X; mk Keys.C; mk Keys.V; mk Keys.Space; mk Keys.Comma; mk Keys.Period; mk Keys.Slash; mk Keys.RightShift|]
                [|mk Keys.CapsLock; mk Keys.Q; mk Keys.W; mk Keys.E; mk Keys.V; mk Keys.Space; mk Keys.K; mk Keys.L; mk Keys.Semicolon; mk Keys.Apostrophe|]
            |]

            Preset1 = Setting.simple None
            Preset2 = Setting.simple None
            Preset3 = Setting.simple None

            VanishingNotes = Setting.simple false
            AutoCalibrateOffset = Setting.simple false
            AdvancedRecommendations = Setting.simple false
            ScoreGraphMode = Setting.simple ScoreGraphMode.Combo
        }

    let mutable internal config : Percyqaz.Flux.Windowing.Config = Unchecked.defaultof<_>

    do 
        // Register decoding rules for Percyqaz.Flux config
        JSON.WithAutoCodec<Percyqaz.Flux.Windowing.Config>(false)
            .WithAutoCodec<Percyqaz.Flux.Input.Bind>() |> ignore

    let mutable options : GameOptions = Unchecked.defaultof<_>

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
            
            Hotkeys.register "import" (ctrl Keys.I)
            Hotkeys.register "options" (ctrl Keys.O)
            Hotkeys.register "wiki" (ctrl Keys.H)
            Hotkeys.register "console" (mk Keys.GraveAccent)
            
            Hotkeys.register "library_mode" (mk Keys.D1)
            Hotkeys.register "add_to_collection" (mk Keys.RightBracket)
            Hotkeys.register "remove_from_collection" (mk Keys.LeftBracket)
            Hotkeys.register "move_down_in_collection" (ctrl Keys.RightBracket)
            Hotkeys.register "move_up_in_collection" (ctrl Keys.LeftBracket)
            Hotkeys.register "sort_mode" (mk Keys.D2)
            Hotkeys.register "reverse_sort_mode" (shift Keys.D2)
            Hotkeys.register "group_mode" (mk Keys.D3)
            Hotkeys.register "reverse_group_mode" (shift Keys.D3)
            Hotkeys.register "comment" (mk Keys.F)
            Hotkeys.register "show_comments" (shift Keys.F)
            Hotkeys.register "context_menu" (mk Keys.Period)
            Hotkeys.register "practice_mode" (mk Keys.V)
            Hotkeys.register "accept_suggestion" (mk Keys.Tab)

            Hotkeys.register "uprate" (mk Keys.Equal)
            Hotkeys.register "downrate" (mk Keys.Minus)
            Hotkeys.register "uprate_half" (ctrl Keys.Equal)
            Hotkeys.register "downrate_half" (ctrl Keys.Minus)
            Hotkeys.register "uprate_small" (shift Keys.Equal)
            Hotkeys.register "downrate_small" (shift Keys.Minus)

            Hotkeys.register "scoreboard_storage" (mk Keys.Q)
            Hotkeys.register "scoreboard_sort" (mk Keys.W)
            Hotkeys.register "scoreboard_filter" (mk Keys.E)

            Hotkeys.register "scoreboard" (mk Keys.Z)
            Hotkeys.register "table" (mk Keys.X)
            Hotkeys.register "collections" (mk Keys.C)

            Hotkeys.register "preview" (mk Keys.A)
            Hotkeys.register "mods" (mk Keys.S)
            Hotkeys.register "ruleset_switch" (mk Keys.D)
            Hotkeys.register "ruleset_picker" (shift Keys.D)
            Hotkeys.register "random_chart" (mk Keys.R)
            Hotkeys.register "autoplay" (ctrl Keys.A)
            Hotkeys.register "reload_themes" (Key (Keys.S, (true, true, true)))

            Hotkeys.register "skip" (mk Keys.Space)
            Hotkeys.register "retry" (ctrl Keys.R)

            Hotkeys.register "preset1" (ctrl Keys.F1)
            Hotkeys.register "preset2" (ctrl Keys.F2)
            Hotkeys.register "preset3" (ctrl Keys.F3)

            options <- { options with Hotkeys = Hotkeys.import d }

    let private configPath = Path.GetFullPath "config.json"
    let firstLaunch = not (File.Exists configPath)

    module HUDOptions =
        
        open Prelude.Data.Content.HUD
        
        let private cache = Dictionary<string, obj>()
        
        let private load_id<'T>() =
            let id = typeof<'T>.Name
            cache.Remove(id) |> ignore

            let path = Path.Combine(getDataPath "Data", "HUD", id + ".json")
            if File.Exists path then
                match JSON.FromFile<'T>(path) with
                | Ok v -> cache.Add(id, v)
                | Error e ->
                    Logging.Error(sprintf "Error while loading config for gameplay widget '%s'\n  If you edited the file manually, you may have made a mistake or need to close the file in your text editor" id)
                    cache.Add(id, JSON.Default<'T>())
            else 
                let default_value = JSON.Default<'T>()
                JSON.ToFile(path, true) default_value
                cache.Add(id, default_value)
        
        let load() =
            Directory.CreateDirectory(Path.Combine(getDataPath "Data", "HUD")) |> ignore

            load_id<AccuracyMeter>()
            load_id<HitMeter>()
            load_id<Combo>()
            load_id<SkipButton>()
            load_id<ProgressMeter>()
            load_id<Pacemaker>()
            load_id<JudgementCounts>()
            load_id<JudgementMeter>()
            load_id<EarlyLateMeter>()
                    
        let get<'T>() = 
            let id = typeof<'T>.Name
            if cache.ContainsKey id then
                cache.[id] :?> 'T
            else failwithf "config not loaded: %s" id
        
        let set<'T>(value: 'T) =
            let id = typeof<'T>.Name
            cache.[id] <- value
            JSON.ToFile(Path.Combine(getDataPath "Data", "HUD", id + ".json"), true) value

    module Presets =

        let create(name: string) : Preset =
            {
                Name = name
                Locked = false

                VisualOffset = options.VisualOffset.Value
                ScrollSpeed = options.ScrollSpeed.Value
                HitPosition = options.HitPosition.Value
                Upscroll = options.Upscroll.Value
                LaneCover = options.LaneCover
                Noteskin = options.Noteskin.Value
            }

        let load(preset: Preset) =
            options.VisualOffset.Set preset.VisualOffset
            options.ScrollSpeed.Set preset.ScrollSpeed
            options.HitPosition.Set preset.HitPosition
            options.Upscroll.Set preset.Upscroll
            options.LaneCover.LoadPreset(preset.LaneCover)
            if Content.Noteskins.loaded.ContainsKey preset.Noteskin then
                options.Noteskin.Set preset.Noteskin
                Content.Noteskins.Current.switch preset.Noteskin
            else Logging.Error(sprintf "Noteskin '%s' use in this preset has been moved or isn't available" preset.Noteskin)

    let load(instance: int) =
        config <- loadImportantJsonFile "Config" configPath true
        Localisation.loadFile config.Locale
        if config.WorkingDirectory <> "" then Directory.SetCurrentDirectory config.WorkingDirectory
        if instance > 0 then
            let newPath = (Directory.GetCurrentDirectory() + "-instance" + instance.ToString())
            Directory.CreateDirectory newPath |> ignore
            Directory.SetCurrentDirectory newPath
            Logging.Info(sprintf "DEV MODE MULTIPLE INSTANCE: %s" (Directory.GetCurrentDirectory()))
        options <- loadImportantJsonFile "Options" (Path.Combine(getDataPath "Data", "options.json")) true
        
        Directory.CreateDirectory (getDataPath "Songs") |> ignore
        // todo: similar how tos in Rulesets, Themes, Noteskins
        File.WriteAllText(Path.Combine(getDataPath "Songs", "HOW_TO_ADD_SONGS.txt"),
            "Dragging and dropping things into this folder won't work.\n" +
            "Instead, drag and drop things onto the *game window* while it's open and it will import.\n" +
            "Does this folder have stuff in, but they don't show up in game?\n" +
            "Check they are .yav files, and then go to Options > Debug > Rebuild cache and let that run.")

        HUDOptions.load()

    let save() =
        try
            JSON.ToFile(configPath, true) config
            JSON.ToFile(Path.Combine(getDataPath "Data", "options.json"), true) options
        with err -> Logging.Critical("Failed to write options/config to file.", err)