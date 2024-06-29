namespace Interlude

open System.IO
open System.Collections.Generic
open OpenTK.Windowing.GraphicsLibraryFramework
open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Flux.Input
open Percyqaz.Flux.Input.Bind
open Prelude
open Prelude.Gameplay.Mods
open Prelude.Data.Library.Sorting
open Prelude.Data.Library
open Prelude.Skinning.Noteskins
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
    type PacemakerMode =
        | Accuracy
        | Lamp

    [<Json.AutoCodec>]
    [<RequireQualifiedAccess>]
    type PacemakerSettings =
        {
            Accuracy: float
            Lamp: int
            UsePersonalBest: bool
            Mode: PacemakerMode
        }
        static member Default =
            {
                Accuracy = 0.95
                Lamp = 0
                UsePersonalBest = true
                Mode = PacemakerMode.Accuracy
            }

    [<RequireQualifiedAccess>]
    type ScoreGraphMode =
        | None = 0
        | Combo = 1

    type FailType =
        | Instant = 0
        | EndOfSong = 1

    [<Json.AutoCodec(false)>]
    type LaneCoverOptions =
        {
            Enabled: Setting<bool>
            DrawUnderReceptors: Setting<bool>
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
        static member Default =
            {
                Enabled = Setting.simple false
                DrawUnderReceptors = Setting.simple false
                Sudden = Setting.percentf 0.0f
                Hidden = Setting.percentf 0.45f
                FadeLength = Setting.bounded 200f 0f 500f
                Color = Setting.simple Color.Black
            }

    [<Json.AutoCodec>]
    [<RequireQualifiedAccess>]
    type PresetMode =
        | Unlocked
        | Locked
        | Autosave

    [<Json.AutoCodec(false)>]
    type Preset =
        {
            Name: string
            Mode: PresetMode

            VisualOffset: float32
            ScrollSpeed: float32
            HitPosition: float32
            Upscroll: bool
            LaneCover: LaneCoverOptions
            Noteskin: string
            HUD: string
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
            Noteskin: Setting<string>
            SelectedHUD: Setting<string>
            SelectedRuleset: Setting<string>

            FailCondition: Setting<FailType>
            Pacemaker: Dictionary<string, PacemakerSettings>
            EnablePacemaker: Setting<bool>
            SaveScoreIfUnderPace: Setting<bool>
            OnlySaveNewRecords: Setting<bool>
            SelectedMods: Setting<ModState>

            OsuMount: Setting<Imports.MountedChartSource option>
            QuaverMount: Setting<Imports.MountedChartSource option>
            StepmaniaMount: Setting<Imports.MountedChartSource option>
            EtternaMount: Setting<Imports.MountedChartSource option>

            ChartSortMode: Setting<string>
            ChartSortReverse: Setting<bool>
            ChartGroupMode: Setting<string>
            LibraryMode: Setting<LibraryMode>
            ChartGroupReverse: Setting<bool>
            ScoreSortMode: Setting<int>

            TreeShowGradesOnly: Setting<bool>
            Table: Setting<string option>

            EnableConsole: Setting<bool>
            Hotkeys: Dictionary<Hotkey, Bind>
            GameplayBinds: (Bind array) array

            Preset1: Setting<Preset option>
            Preset2: Setting<Preset option>
            Preset3: Setting<Preset option>
            SelectedPreset: Setting<int option>
            KeymodePreferredPresets: int option array

            VanishingNotes: Setting<bool>
            AutoCalibrateOffset: Setting<bool>
            ScoreGraphMode: Setting<ScoreGraphMode>
            ConfirmExit: Setting<bool>
            HoldToGiveUp: Setting<bool>
        }
        static member Default =
            {
                VisualOffset = Setting.bounded 0.0f -500.0f 500.0f |> Setting.roundf 0
                AudioOffset = Setting.bounded 0.0f -500.0f 500.0f |> Setting.roundf 0
                AudioVolume = Setting.percent 0.05
                CurrentChart = Setting.simple ""
                Theme = Content.Themes.selected_id

                ScrollSpeed = Setting.bounded 2.05f 1.0f 5.0f |> Setting.roundf 2
                HitPosition = Setting.bounded 0.0f -300.0f 600.0f
                HitLighting = Setting.simple false
                Upscroll = Setting.simple false
                BackgroundDim = Setting.percentf 0.5f
                LaneCover = LaneCoverOptions.Default
                Noteskin = Content.Skins.selected_noteskin_id
                SelectedHUD = Content.Skins.selected_hud_id
                SelectedRuleset = Content.Rulesets.selected_id

                FailCondition = Setting.simple FailType.EndOfSong
                Pacemaker = Dictionary<string, PacemakerSettings>()
                EnablePacemaker = Setting.simple false
                SaveScoreIfUnderPace = Setting.simple true
                OnlySaveNewRecords = Setting.simple false
                SelectedMods = Setting.simple Map.empty

                OsuMount = Setting.simple None
                QuaverMount = Setting.simple None
                StepmaniaMount = Setting.simple None
                EtternaMount = Setting.simple None

                ChartSortMode = Setting.simple "Title"
                ChartSortReverse = Setting.simple false
                ChartGroupMode = Setting.simple "Pack"
                LibraryMode = Setting.simple LibraryMode.All
                ChartGroupReverse = Setting.simple false
                ScoreSortMode = Setting.simple 0

                TreeShowGradesOnly = Setting.simple true
                Table = Content.Tables.selected_id

                EnableConsole = Setting.simple false
                Hotkeys = Dictionary<Hotkey, Bind>()
                GameplayBinds =
                    [|
                        [| mk Keys.Left; mk Keys.Down; mk Keys.Right |]
                        [| mk Keys.Z; mk Keys.X; mk Keys.Period; mk Keys.Slash |]
                        [| mk Keys.Z; mk Keys.X; mk Keys.Space; mk Keys.Period; mk Keys.Slash |]
                        [|
                            mk Keys.Z
                            mk Keys.X
                            mk Keys.C
                            mk Keys.Comma
                            mk Keys.Period
                            mk Keys.Slash
                        |]
                        [|
                            mk Keys.Z
                            mk Keys.X
                            mk Keys.C
                            mk Keys.Space
                            mk Keys.Comma
                            mk Keys.Period
                            mk Keys.Slash
                        |]
                        [|
                            mk Keys.Z
                            mk Keys.X
                            mk Keys.C
                            mk Keys.V
                            mk Keys.Comma
                            mk Keys.Period
                            mk Keys.Slash
                            mk Keys.RightShift
                        |]
                        [|
                            mk Keys.Z
                            mk Keys.X
                            mk Keys.C
                            mk Keys.V
                            mk Keys.Space
                            mk Keys.Comma
                            mk Keys.Period
                            mk Keys.Slash
                            mk Keys.RightShift
                        |]
                        [|
                            mk Keys.CapsLock
                            mk Keys.Q
                            mk Keys.W
                            mk Keys.E
                            mk Keys.V
                            mk Keys.Space
                            mk Keys.K
                            mk Keys.L
                            mk Keys.Semicolon
                            mk Keys.Apostrophe
                        |]
                    |]

                Preset1 = Setting.simple None
                Preset2 = Setting.simple None
                Preset3 = Setting.simple None
                SelectedPreset = Setting.simple None
                KeymodePreferredPresets = [| None; None; None; None; None; None; None; None |]

                VanishingNotes = Setting.simple true
                AutoCalibrateOffset = Setting.simple false
                ScoreGraphMode = Setting.simple ScoreGraphMode.Combo
                ConfirmExit = Setting.simple true
                HoldToGiveUp = Setting.simple false
            }

    let mutable internal config: Percyqaz.Flux.Windowing.Config = Unchecked.defaultof<_>

    let mutable options: GameOptions = Unchecked.defaultof<_>

    module Hotkeys =

        let init_startup (d: Dictionary<Hotkey, Bind>) =
            Hotkeys.register "search" (mk Keys.Tab)
            Hotkeys.register "toolbar" (ctrl Keys.T)
            Hotkeys.register "tooltip" (mk Keys.Slash)
            Hotkeys.register "delete" (mk Keys.Delete)
            Hotkeys.register "screenshot" (mk Keys.F12)
            Hotkeys.register "volume" (mk Keys.LeftAlt)
            Hotkeys.register "volume_up" (alt Keys.Up)
            Hotkeys.register "volume_down" (alt Keys.Down)
            Hotkeys.register "previous" (mk Keys.Left)
            Hotkeys.register "next" (mk Keys.Right)
            Hotkeys.register "previous_group" (mk Keys.PageUp)
            Hotkeys.register "next_group" (mk Keys.PageDown)
            Hotkeys.register "start" (mk Keys.Home)
            Hotkeys.register "end" (mk Keys.End)
            Hotkeys.register "endless_mode" (shift Keys.E)
            Hotkeys.register "pause_music" (Mouse MouseButton.Middle)

            Hotkeys.register "import" (ctrl Keys.I)
            Hotkeys.register "options" (ctrl Keys.O)
            Hotkeys.register "wiki" (ctrl Keys.H)
            Hotkeys.register "console" (mk Keys.GraveAccent)
            Hotkeys.register "edit_noteskin" (ctrl Keys.E)
            Hotkeys.register "player_list" (mk Keys.F9)

            Hotkeys.register "library_mode" (mk Keys.D1)
            Hotkeys.register "add_to_collection" (mk Keys.RightBracket)
            Hotkeys.register "remove_from_collection" (mk Keys.LeftBracket)
            Hotkeys.register "move_down_in_playlist" (ctrl Keys.Down)
            Hotkeys.register "move_up_in_playlist" (ctrl Keys.Up)
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
            Hotkeys.register "random_chart" (mk Keys.R)
            Hotkeys.register "previous_random_chart" (shift Keys.R)
            Hotkeys.register "autoplay" (ctrl Keys.A)
            Hotkeys.register "reload_themes" (Key(Keys.S, (true, true, true)))

            Hotkeys.register "skip" (mk Keys.Space)
            Hotkeys.register "retry" (ctrl Keys.R)
            Hotkeys.register "offset" (ctrl Keys.O)

            Hotkeys.register "preset1" (ctrl Keys.F1)
            Hotkeys.register "preset2" (ctrl Keys.F2)
            Hotkeys.register "preset3" (ctrl Keys.F3)

            options <-
                { options with
                    Hotkeys = Hotkeys.import d
                }

    let private CONFIG_PATH = Path.GetFullPath "config.json"
    let first_launch = not (File.Exists CONFIG_PATH)

    module Presets =

        let get (id: int) =
            [| options.Preset1; options.Preset2; options.Preset3 |].[id - 1]

        let create (name: string) : Preset =
            {
                Name = name
                Mode = PresetMode.Unlocked

                VisualOffset = options.VisualOffset.Value
                ScrollSpeed = options.ScrollSpeed.Value
                HitPosition = options.HitPosition.Value
                Upscroll = options.Upscroll.Value
                LaneCover = options.LaneCover
                Noteskin = options.Noteskin.Value
                HUD = options.SelectedHUD.Value
            }

        let save (preset: Preset) : Preset =
            { preset with
                VisualOffset = options.VisualOffset.Value
                ScrollSpeed = options.ScrollSpeed.Value
                HitPosition = options.HitPosition.Value
                Upscroll = options.Upscroll.Value
                LaneCover = options.LaneCover
                Noteskin = options.Noteskin.Value
                HUD = options.SelectedHUD.Value
            }

        let load (id: int) =
            match options.SelectedPreset.Value with
            | None -> ()
            | Some i ->
                let setting = get i

                match setting.Value with
                | Some preset when preset.Mode = PresetMode.Autosave -> setting.Set(Some(save preset))
                | _ -> ()

            match (get id).Value with
            | Some loaded_preset ->
                options.SelectedPreset.Value <- Some id

                options.VisualOffset.Set loaded_preset.VisualOffset
                options.ScrollSpeed.Set loaded_preset.ScrollSpeed
                options.HitPosition.Set loaded_preset.HitPosition
                options.Upscroll.Set loaded_preset.Upscroll
                options.LaneCover.LoadPreset loaded_preset.LaneCover

                if Content.Skins.noteskin_exists loaded_preset.Noteskin then
                    options.Noteskin.Set loaded_preset.Noteskin
                else
                    Logging.Debug(
                        sprintf
                            "Noteskin '%s' used in this preset has been renamed or isn't available"
                            loaded_preset.Noteskin
                    )

                if Content.Skins.hud_exists loaded_preset.HUD then
                    options.SelectedHUD.Set loaded_preset.HUD
                else
                    Logging.Debug(
                        sprintf
                            "HUD '%O' used in this preset has been renamed or isn't available"
                            loaded_preset.HUD
                    )

                Some loaded_preset.Name
            | None -> None

        let keymode_changed (keys: int) =
            match options.KeymodePreferredPresets.[keys - 3] with
            | Some preference -> load preference |> ignore
            | None -> ()

    let init_startup (instance: int) =
        // Register decoding rules for Percyqaz.Flux config
        JSON
            .WithAutoCodec<Percyqaz.Flux.Windowing.Config>(false)
            .WithAutoCodec<Percyqaz.Flux.Windowing.FullscreenVideoMode>()
            .WithAutoCodec<Percyqaz.Flux.Input.Bind>()
        |> ignore

        config <- load_important_json_file "Config" CONFIG_PATH true
        Localisation.load_file config.Locale

        if config.WorkingDirectory <> "" then
            Directory.SetCurrentDirectory config.WorkingDirectory

        if instance > 0 then
            let new_path = (Directory.GetCurrentDirectory() + "-instance" + instance.ToString())
            Directory.CreateDirectory new_path |> ignore
            Directory.SetCurrentDirectory new_path
            Logging.Info(sprintf "DEV MODE MULTIPLE INSTANCE: %s" (Directory.GetCurrentDirectory()))

        options <- load_important_json_file "Options" (Path.Combine(get_game_folder "Data", "options.json")) true

        Directory.CreateDirectory(get_game_folder "Songs") |> ignore
        // todo: similar how tos in Rulesets, Themes, Noteskins
        File.WriteAllText(
            Path.Combine(get_game_folder "Songs", "HOW_TO_ADD_SONGS.txt"),
            "Dragging and dropping things into this folder won't work.\n"
            + "Instead, drag and drop things onto the *game window* while it's open and it will import, OR use the ingame downloaders.\n"
            + "> Help! I have files in here, but they don't show up ingame?\n"
            + "Make sure they are .yav files, if so go to Options > Debug > Rebuild cache and let that run, it will re-add anything that's missing."
        )

    let deinit () =
        try
            save_important_json_file CONFIG_PATH config
            save_important_json_file (Path.Combine(get_game_folder "Data", "options.json")) options
        with err ->
            Logging.Critical("Failed to write options/config to file.", err)
