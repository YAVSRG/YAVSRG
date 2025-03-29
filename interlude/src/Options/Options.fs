namespace Interlude.Options

open System.IO
open System.Collections.Generic
open Percyqaz.Common
open Percyqaz.Data
open Percyqaz.Flux.Input
open Percyqaz.Flux.Input.Bind
open Percyqaz.Flux.Windowing
open Prelude
open Prelude.Mods
open Prelude.Data.Library
open Interlude.Content

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
type PacemakerPersonalBestMode =
    | Never
    | IfBetter
    | Always

[<Json.AutoCodec>]
[<RequireQualifiedAccess>]
type QuitOutBehaviour =
    | SaveAndShow
    | Show
    | Ignore

[<Json.AutoCodec(false)>]
[<RequireQualifiedAccess>]
type PacemakerSettings =
    {
        Accuracy: float
        Lamp: int
        PersonalBest: PacemakerPersonalBestMode
        Mode: PacemakerMode
    }
    static member Default =
        {
            Accuracy = 0.95
            Lamp = 0
            PersonalBest = PacemakerPersonalBestMode.IfBetter
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
            FadeLength = Setting.bounded (0f, 500f) 200f
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

        VisualOffset: GameplayTime
        ScrollSpeed: float32<rate/ms>
        HitPosition: float32
        Upscroll: bool
        LaneCover: LaneCoverPresetOptions
        Noteskin: string
        HUD: string
    }

[<Json.AutoCodec(false)>]
type GameOptions =
    {
        Language: Setting<string>

        VisualOffset: Setting.Bounded<GameplayTime>
        AudioOffset: Setting.Bounded<GameplayTime>
        AudioVolume: Setting.Bounded<float>
        AudioPitchRates: Setting<bool>
        CurrentChart: Setting<string>
        Theme: Setting<string>

        ScrollSpeed: Setting.Bounded<float32<rate/ms>>
        HitPosition: Setting.Bounded<float32>
        HitLighting: Setting<bool>
        Upscroll: Setting<bool>
        BackgroundDim: Setting.Bounded<float32>
        LaneCover: LaneCoverOptions
        Noteskin: Setting<string>
        SelectedHUD: Setting<string>
        SelectedRuleset: Setting<string>

        Pacemaker: Dictionary<string, PacemakerSettings>
        EnablePacemaker: Setting<bool>
        EnablePacemakerFailMidway: Setting<bool>
        QuitOutBehaviour: Setting<QuitOutBehaviour>
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
        TreeShowTimestamps: Setting<bool>
        TreeAlwaysShowCollections: Setting<bool>
        TreeShowNativeText: Setting<bool>
        Table: Setting<string option>
        SuggestionsMinRate: Setting.Bounded<Rate>
        SuggestionsMaxRate: Setting.Bounded<Rate>
        SuggestionsEnableRates: Setting<bool>
        SuggestionsOnlyNew: Setting<bool>

        EnableExperiments: Setting<bool>
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
        HoldToGiveUp: Setting<bool>
        MenusMuffleSong: Setting<bool>
    }
    static member Default =
        {
            Language = Setting.simple "en_GB"

            VisualOffset = 0.0f<ms / rate> |> Setting.bounded  (-500.0f<ms / rate>, 500.0f<ms / rate>) |> Setting.roundf_uom 0
            AudioOffset =  0.0f<ms / rate> |> Setting.bounded (-500.0f<ms / rate>, 500.0f<ms / rate>) |> Setting.roundf_uom 0
            AudioVolume = Setting.bounded (0.0, 1.0) 0.05
            AudioPitchRates = Setting.simple true
            CurrentChart = Setting.simple ""
            Theme = Themes.selected_id

            ScrollSpeed = 2.05f<rate/ms> |> Setting.bounded (1.0f<rate/ms>, 5.0f<rate/ms>) |> Setting.roundf_uom 2
            HitPosition = 0.0f |> Setting.bounded (-300.0f, 600.0f)
            HitLighting = Setting.simple false
            Upscroll = Setting.simple false
            BackgroundDim = Setting.percentf 0.5f
            LaneCover = LaneCoverOptions.Default
            Noteskin = Skins.selected_noteskin_id
            SelectedHUD = Skins.selected_hud_id
            SelectedRuleset = Rulesets.selected_id

            Pacemaker = Dictionary<string, PacemakerSettings>()
            EnablePacemaker = Setting.simple false
            EnablePacemakerFailMidway = Setting.simple true
            QuitOutBehaviour = Setting.simple QuitOutBehaviour.SaveAndShow
            OnlySaveNewRecords = Setting.simple false
            SelectedMods = Setting.simple Map.empty

            OsuMount = Setting.simple None
            QuaverMount = Setting.simple None
            StepmaniaMount = Setting.simple None
            EtternaMount = Setting.simple None

            ChartSortMode = Setting.simple "title"
            ChartSortReverse = Setting.simple false
            ChartGroupMode = Setting.simple "pack"
            ChartGroupReverse = Setting.simple false
            ScoreSortMode = Setting.simple 0
            TreeShowGradesOnly = Setting.simple true
            TreeShowTimestamps = Setting.simple false
            TreeAlwaysShowCollections = Setting.simple false
            TreeShowNativeText = Setting.simple false
            Table = Tables.selected_id
            SuggestionsMinRate = Setting.rate 1.0f<rate>
            SuggestionsMaxRate = Setting.rate 1.5f<rate>
            SuggestionsEnableRates = Setting.simple true
            SuggestionsOnlyNew = Setting.simple true

            EnableExperiments = Setting.simple false
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
            HoldToGiveUp = Setting.simple false
            MenusMuffleSong = Setting.simple true
        }

[<AutoOpen>]
module StaticOptions =

    let mutable internal config: WindowSettings = Unchecked.defaultof<_>
    let mutable options: GameOptions = Unchecked.defaultof<_>

module Options =

    let private CONFIG_PATH = Path.GetFullPath "config.json"

    let load_window_config (instance: int) () : WindowOptions =
        // Register decoding rules for Percyqaz.Flux config
        JSON
            .WithAutoCodec<FullscreenVideoMode>()
            .WithAutoCodec<Bind>()
        |> ignore

        config <- load_important_json_file "Config" CONFIG_PATH true

        if config.WorkingDirectory <> "" then
            Directory.SetCurrentDirectory config.WorkingDirectory

        if instance > 0 then
            let new_path = (Directory.GetCurrentDirectory() + "-instance" + instance.ToString())
            Directory.CreateDirectory new_path |> ignore
            Directory.SetCurrentDirectory new_path
            Logging.Info "DEV MODE MULTIPLE INSTANCE: %s" (Directory.GetCurrentDirectory())

        config.ToOptions

    let init() : unit =
        options <- load_important_json_file "Options" (Path.Combine(get_game_folder "Data", "options.json")) true
        options <- { options with Hotkeys = Hotkeys.init options.Hotkeys }
        Localisation.load_language options.Language.Value

    let deinit () : unit =
        try
            save_important_json_file CONFIG_PATH config
            save_important_json_file (Path.Combine(get_game_folder "Data", "options.json")) options
        with err ->
            Logging.Critical "Failed to write options/config to file.\n%O" err