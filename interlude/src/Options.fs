namespace Interlude

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Flux.Input
open Percyqaz.Flux.Input.Bind
open Prelude
open Prelude.Gameplay.Mods
open Prelude.Data.Library
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
    type ScoreGraphLineMode =
        | None = 0
        | Combo = 1
        | Mean = 2
        | StandardDeviation = 3
        | Accuracy = 4
        | MA = 5
        | PA = 6
        
    [<RequireQualifiedAccess>]
    type ScoreGraphLineColor =
        | White = 0
        | Lamp = 1
        | Grade = 2

    type FailType =
        | Instant = 0
        | EndOfSong = 1

    [<Json.AutoCodec(false)>]
    type LaneCoverPresetOptions =
        {
            Enabled: bool
            DrawUnderReceptors: bool
            Sudden: float32
            Hidden: float32
            FadeLength: float32
            Color: Color
        }

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
        member this.LoadPreset(p: LaneCoverPresetOptions) =
            this.Enabled.Set p.Enabled
            this.DrawUnderReceptors.Set p.DrawUnderReceptors
            this.Sudden.Set p.Sudden
            this.Hidden.Set p.Hidden
            this.FadeLength.Set p.FadeLength
            this.Color.Set p.Color
        member this.ToPreset : LaneCoverPresetOptions =
            {
                Enabled = this.Enabled.Value
                DrawUnderReceptors = this.DrawUnderReceptors.Value
                Sudden = this.Sudden.Value
                Hidden = this.Hidden.Value
                FadeLength = this.FadeLength.Value
                Color = this.Color.Value
            }
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
            LaneCover: LaneCoverPresetOptions
            Noteskin: string
            HUD: string
        }

    [<Json.AutoCodec(false)>]
    type GameOptions =
        {
            VisualOffset: Setting.Bounded<float32> // todo: change to ms / rate
            AudioOffset: Setting.Bounded<float32>
            AudioVolume: Setting.Bounded<float>
            AudioPitchRates: Setting<bool>
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
            ChartGroupReverse: Setting<bool>
            ScoreSortMode: Setting<int>
            TreeShowGradesOnly: Setting<bool>
            TreeAlwaysShowCollections: Setting<bool>
            TreeShowNativeText: Setting<bool>
            Table: Setting<string option>
            SuggestionsMinRate: Setting.Bounded<Rate>
            SuggestionsMaxRate: Setting.Bounded<Rate>
            SuggestionsOnlyNew: Setting<bool>

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
            ScoreGraphLineMode: Setting<ScoreGraphLineMode>
            ScoreGraphLineColor: Setting<ScoreGraphLineColor>
            ScoreGraphLineOnTop: Setting<bool>
            ScoreGraphWindowBackground: Setting<bool>
            ConfirmExit: Setting<bool>
            HoldToGiveUp: Setting<bool>
            MenusMuffleSong: Setting<bool>
        }
        static member Default =
            {
                VisualOffset = Setting.bounded 0.0f -500.0f 500.0f |> Setting.roundf 0
                AudioOffset = Setting.bounded 0.0f -500.0f 500.0f |> Setting.roundf 0
                AudioVolume = Setting.percent 0.05
                AudioPitchRates = Setting.simple true
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
                ChartGroupReverse = Setting.simple false
                ScoreSortMode = Setting.simple 0
                TreeShowGradesOnly = Setting.simple true
                TreeAlwaysShowCollections = Setting.simple false
                TreeShowNativeText = Setting.simple false
                Table = Content.Tables.selected_id
                SuggestionsMinRate = Setting.rate 1.0f<rate>
                SuggestionsMaxRate = Setting.rate 1.5f<rate>
                SuggestionsOnlyNew = Setting.simple true

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
                ScoreGraphLineMode = Setting.simple ScoreGraphLineMode.Combo
                ScoreGraphLineColor = Setting.simple ScoreGraphLineColor.Lamp
                ScoreGraphLineOnTop = Setting.simple false
                ScoreGraphWindowBackground = Setting.simple false
                ConfirmExit = Setting.simple true
                HoldToGiveUp = Setting.simple false
                MenusMuffleSong = Setting.simple true
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
            Hotkeys.register "pause_music" (Bind.Mouse MouseButton.Middle)

            Hotkeys.register "import" (ctrl Keys.I)
            Hotkeys.register "options" (ctrl Keys.O)
            Hotkeys.register "wiki" (ctrl Keys.H)
            Hotkeys.register "console" (mk Keys.GraveAccent)
            Hotkeys.register "player_list" (mk Keys.F9)
            Hotkeys.register "quick_menu" (ctrl Keys.Tab)

            Hotkeys.register "level_select_options" (mk Keys.D1)
            Hotkeys.register "like" (mk Keys.RightBracket)
            Hotkeys.register "unlike" (mk Keys.LeftBracket)
            Hotkeys.register "move_down_in_playlist" (ctrl Keys.Down)
            Hotkeys.register "move_up_in_playlist" (ctrl Keys.Up)
            Hotkeys.register "sort_mode" (mk Keys.D2)
            Hotkeys.register "reverse_sort_mode" (shift Keys.D2)
            Hotkeys.register "group_mode" (mk Keys.D3)
            Hotkeys.register "reverse_group_mode" (shift Keys.D3)
            Hotkeys.register "context_menu" (mk Keys.Period)
            Hotkeys.register "practice_mode" (mk Keys.V)
            Hotkeys.register "scoreboard" (mk Keys.Z)
            Hotkeys.register "scoreboard_storage" (mk Keys.Q)
            Hotkeys.register "scoreboard_sort" (mk Keys.W)
            Hotkeys.register "scoreboard_filter" (mk Keys.E)
            Hotkeys.register "table" (mk Keys.X)
            Hotkeys.register "collections" (mk Keys.C)
            Hotkeys.register "clear_multi_select" (shift Keys.X)
            Hotkeys.register "multi_select" (shift Keys.Z)
            Hotkeys.register "group_multi_select" (ctrl_shift Keys.Z)

            Hotkeys.register "accept_offset" (mk Keys.Tab)
            Hotkeys.register "reset_offset" (mk Keys.Backspace)
            Hotkeys.register "hide_replay_overlay" (mk Keys.H)

            Hotkeys.register "uprate_big" (ctrl_shift Keys.Equal)
            Hotkeys.register "downrate_big" (ctrl_shift Keys.Minus)
            Hotkeys.register "uprate" (mk Keys.Equal)
            Hotkeys.register "downrate" (mk Keys.Minus)
            Hotkeys.register "uprate_half" (ctrl Keys.Equal)
            Hotkeys.register "downrate_half" (ctrl Keys.Minus)
            Hotkeys.register "uprate_small" (shift Keys.Equal)
            Hotkeys.register "downrate_small" (shift Keys.Minus)

            Hotkeys.register "preview" (mk Keys.A)
            Hotkeys.register "mods" (mk Keys.S)
            Hotkeys.register "ruleset_switch" (mk Keys.D)
            Hotkeys.register "random_chart" (mk Keys.R)
            Hotkeys.register "previous_random_chart" (shift Keys.R)
            Hotkeys.register "autoplay" (ctrl Keys.A)
            Hotkeys.register "reload_content" (Bind.Key(Keys.S, (true, true, true)))

            Hotkeys.register "skip" (mk Keys.Space)
            Hotkeys.register "retry" (ctrl Keys.R)
            Hotkeys.register "next_song" (ctrl_shift Keys.R)
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
                Mode = PresetMode.Autosave

                VisualOffset = options.VisualOffset.Value
                ScrollSpeed = options.ScrollSpeed.Value
                HitPosition = options.HitPosition.Value
                Upscroll = options.Upscroll.Value
                LaneCover = options.LaneCover.ToPreset
                Noteskin = options.Noteskin.Value
                HUD = options.SelectedHUD.Value
            }

        let save (preset: Preset) : Preset =
            { preset with
                VisualOffset = options.VisualOffset.Value
                ScrollSpeed = options.ScrollSpeed.Value
                HitPosition = options.HitPosition.Value
                Upscroll = options.Upscroll.Value
                LaneCover = options.LaneCover.ToPreset
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

    let deinit () =
        try
            save_important_json_file CONFIG_PATH config
            save_important_json_file (Path.Combine(get_game_folder "Data", "options.json")) options
        with err ->
            Logging.Critical("Failed to write options/config to file.", err)
