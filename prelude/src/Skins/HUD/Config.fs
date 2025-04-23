namespace Prelude.Skins.HudLayouts

open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Gameplay.Rulesets
open Prelude.Skins

[<RequireQualifiedAccess>]
type HudElement =
    | Accuracy
    | ErrorBar
    | Combo
    | SkipButton
    | Judgement
    | EarlyLate
    | ProgressPie
    | JudgementCounter
    | RateMods
    | BPM
    | Pacemaker
    | InputMeter
    | KeysPerSecond
    | CustomImage
    static member FULL_LIST =
        [
            Accuracy
            ErrorBar
            Combo
            SkipButton
            Judgement
            EarlyLate
            ProgressPie
            JudgementCounter
            RateMods
            BPM
            Pacemaker
            InputMeter
            KeysPerSecond
            CustomImage
        ]

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type JudgementDisplayType =
    | Name
    | Texture of int

type ProgressPieLabel =
    | None = 0
    | Countdown = 1
    | Percentage = 2

type ErrorBarMovingAverageType =
    | None = 0
    | Arrow = 1
    | ReplaceBars = 2

type ErrorBarRotation =
    | Normal = 0
    | Clockwise = 1
    | Anticlockwise = 2

[<Json.AutoCodec(false)>]
type HudPosition =
    {
        RelativeToPlayfield: bool
        Left: float32 * float32
        Top: float32 * float32
        Right: float32 * float32
        Bottom: float32 * float32
    }
    member this.TextAlignment =
        let lo, la = this.Left
        let ro, _ = this.Right
        if la = 0.5f then
            0.5f
        elif la < 0.5f then
            if lo >= 0.0f then 0.0f
            elif ro <= 0.0f then 1.0f
            else 0.5f
        else
            if lo >= 0.0f then 0.0f
            elif ro <= 0.0f then 1.0f
            else 0.5f

    member this.Rotate =
        let inline pointwise_add (a, b) (c, d) = (a + c, b + d)
        let inline pointwise_subtract (a, b) (c, d) = (a - c, b - d)
        let inline mult m (a, b) = (a * m, b * m)
        let center_x = pointwise_add this.Left this.Right |> mult 0.5f
        let center_y = pointwise_add this.Top this.Bottom |> mult 0.5f
        let hwidth = pointwise_subtract this.Right this.Left |> mult 0.5f
        let hheight = pointwise_subtract this.Bottom this.Top |> mult 0.5f

        {
            RelativeToPlayfield = this.RelativeToPlayfield
            Left = pointwise_subtract center_x hheight
            Top = pointwise_subtract center_y hwidth
            Right = pointwise_add center_x hheight
            Bottom = pointwise_add center_y hwidth
        }

    member this.FlipVertical =
        { this with
            Top = (-fst this.Bottom, 1.0f - snd this.Bottom)
            Bottom = (-fst this.Top, 1.0f - snd this.Top)
        }

    member this.FlipHorizontal =
        { this with
            Left = (-fst this.Right, 1.0f - snd this.Right)
            Right = (-fst this.Left, 1.0f - snd this.Left)
        }

[<Json.AutoCodec(false)>]
type BackgroundTextureOptions =
    {
        Enable: bool
        Scale: float32
        AlignmentX: float32
        AlignmentY: float32
    }

[<Json.AutoCodec(false)>]
type HudConfig =
    {
        AccuracyEnabled: bool
        AccuracyPosition: HudPosition
        AccuracyGradeColors: bool
        AccuracyShowName: bool
        AccuracyUseFont: bool
        AccuracyFontSpacing: float32
        AccuracyDotExtraSpacing: float32
        AccuracyPercentExtraSpacing: float32

        TimingDisplayEnabled: bool
        TimingDisplayPosition: HudPosition
        TimingDisplayFadeTime: float32<ms / rate>
        TimingDisplayThickness: float32
        TimingDisplayShowGuide: bool
        TimingDisplayGuideThickness: float32
        TimingDisplayShowNonJudgements: bool
        TimingDisplayReleasesExtraHeight: float32
        TimingDisplayWindowsOpacity: float32
        TimingDisplayHalfScaleReleases: bool
        TimingDisplayMovingAverageType: ErrorBarMovingAverageType
        TimingDisplayMovingAverageSensitivity: float32
        TimingDisplayMovingAverageColor: Color
        TimingDisplayRotation: ErrorBarRotation

        ComboEnabled: bool
        ComboPosition: HudPosition
        ComboLampColors: bool
        ComboGrowth: float32
        ComboPop: float32
        ComboUseFont: bool
        ComboFontSpacing: float32

        SkipButtonPosition: HudPosition
        SkipButtonBackground: BackgroundTextureOptions

        JudgementMeterEnabled: bool
        JudgementMeterPosition: HudPosition
        JudgementMeterIgnorePerfect: bool
        JudgementMeterPrioritiseLower: bool
        JudgementMeterDuration: float32<ms / rate>
        JudgementMeterFrameTime: float32<ms / rate>
        JudgementMeterUseTexture: bool
        JudgementMeterUseBuiltInAnimation: bool
        JudgementMeterCustomDisplay: Map<int, JudgementDisplayType array>

        EarlyLateMeterEnabled: bool
        EarlyLateMeterPosition: HudPosition
        EarlyLateMeterDuration: float32<ms / rate>
        EarlyLateMeterUseTexture: bool
        EarlyLateMeterFrameTime: float32<ms / rate>
        EarlyLateMeterEarlyText: string
        EarlyLateMeterLateText: string
        EarlyLateMeterEarlyColor: Color
        EarlyLateMeterLateColor: Color

        ProgressMeterEnabled: bool
        ProgressMeterPosition: HudPosition
        ProgressMeterLabel: ProgressPieLabel
        ProgressMeterColor: Color
        ProgressMeterBackgroundColor: Color
        ProgressMeterLabelSize: float32
        ProgressMeterUseFont: bool
        ProgressMeterFontSpacing: float32
        ProgressMeterColonExtraSpacing: float32
        ProgressMeterPercentExtraSpacing: float32

        PacemakerPosition: HudPosition

        JudgementCounterEnabled: bool
        JudgementCounterPosition: HudPosition
        JudgementCounterFadeTime: float32<ms / rate>
        JudgementCounterPopAmount: float32
        JudgementCounterOpacity: float32
        JudgementCounterTextScale: float32
        JudgementCounterUseFont: bool
        JudgementCounterFontSpacing: float32
        JudgementCounterDotExtraSpacing: float32
        JudgementCounterColonExtraSpacing: float32
        JudgementCounterShowRatio: bool
        JudgementCounterShowLabels: bool
        JudgementCounterCustomDisplay: Map<int, int option array>

        RateModMeterEnabled: bool
        RateModMeterPosition: HudPosition
        RateModMeterShowMods: bool

        BPMMeterEnabled: bool
        BPMMeterPosition: HudPosition

        InputMeterEnabled: bool
        InputMeterPosition: HudPosition
        InputMeterScrollSpeed: float32<rate / ms>
        InputMeterKeyColor: Color
        InputMeterKeyFadeTime: float32<ms / rate>
        InputMeterColumnPadding: float32
        InputMeterShowInputs: bool
        InputMeterInputColor: Color
        InputMeterInputFadeDistance: float32
        InputMeterScrollDownwards: bool
        InputMeterJudgementColors: bool
        InputMeterShowKeybinds: bool
        InputMeterKeybindColor: Color

        KeysPerSecondMeterEnabled: bool
        KeysPerSecondMeterShowAverage: bool
        KeysPerSecondMeterShowMax: bool
        KeysPerSecondMeterShowTotal: bool
        KeysPerSecondMeterPosition: HudPosition

        MultiplayerScoreTrackerPosition: HudPosition

        CustomImageEnabled: bool
        CustomImagePosition: HudPosition
        CustomImageFrameTime: float32<ms / rate>
    }
    static member Default =
        {
            AccuracyEnabled = true
            AccuracyPosition =
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = 40.0f, 0.0f
                    Right = 100.0f, 0.5f
                    Bottom = 120.0f, 0.0f
                }
            AccuracyGradeColors = true
            AccuracyShowName = true
            AccuracyUseFont = false
            AccuracyFontSpacing = 0.0f
            AccuracyDotExtraSpacing = 0.0f
            AccuracyPercentExtraSpacing = 0.0f

            TimingDisplayEnabled = true
            TimingDisplayPosition =
                {
                    RelativeToPlayfield = true
                    Left = -300.0f, 0.5f
                    Top = 10.0f, 0.5f
                    Right = 300.0f, 0.5f
                    Bottom = 25.0f, 0.5f
                }
            TimingDisplayFadeTime = 1000.0f<ms / rate>
            TimingDisplayThickness = 5.0f
            TimingDisplayShowGuide = true
            TimingDisplayGuideThickness = 1.0f
            TimingDisplayShowNonJudgements = true
            TimingDisplayReleasesExtraHeight = 5.0f
            TimingDisplayWindowsOpacity = 0.0f
            TimingDisplayHalfScaleReleases = true
            TimingDisplayMovingAverageType = ErrorBarMovingAverageType.None
            TimingDisplayMovingAverageSensitivity = 0.75f
            TimingDisplayMovingAverageColor = Color.Aqua
            TimingDisplayRotation = ErrorBarRotation.Normal

            ComboEnabled = true
            ComboPosition =
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = -65.0f, 0.5f
                    Right = 100.0f, 0.5f
                    Bottom = -5.0f, 0.5f
                }
            ComboLampColors = true
            ComboGrowth = 0.01f
            ComboPop = 5.0f
            ComboUseFont = false
            ComboFontSpacing = 0.0f

            SkipButtonPosition =
                {
                    RelativeToPlayfield = true
                    Left = -200.0f, 0.5f
                    Top = 130.0f, 0.5f
                    Right = 200.0f, 0.5f
                    Bottom = 230.0f, 0.5f
                }
            SkipButtonBackground =
                {
                    Enable = false
                    Scale = 1.1f
                    AlignmentX = 0.5f
                    AlignmentY = 0.5f
                }

            JudgementMeterEnabled = true
            JudgementMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = -128.0f, 0.5f
                    Top = -160.0f, 0.5f
                    Right = 128.0f, 0.5f
                    Bottom = -105.0f, 0.5f
                }
            JudgementMeterIgnorePerfect = false
            JudgementMeterPrioritiseLower = false
            JudgementMeterUseTexture = false
            JudgementMeterUseBuiltInAnimation = true
            JudgementMeterDuration = 300.0f<ms / rate>
            JudgementMeterFrameTime = 50f<ms / rate>
            JudgementMeterCustomDisplay = Map.empty

            EarlyLateMeterEnabled = false
            EarlyLateMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = -128.0f, 0.5f
                    Top = -185.0f, 0.5f
                    Right = 128.0f, 0.5f
                    Bottom = -150.0f, 0.5f
                }
            EarlyLateMeterDuration = 200.0f<ms / rate>
            EarlyLateMeterUseTexture = false
            EarlyLateMeterFrameTime = 20.0f<ms / rate>
            EarlyLateMeterEarlyText = "Early"
            EarlyLateMeterLateText = "Late"
            EarlyLateMeterEarlyColor = Color.FromArgb(52, 79, 235)
            EarlyLateMeterLateColor = Color.FromArgb(235, 52, 52)

            ProgressMeterEnabled = true
            ProgressMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = 120.0f, 0.5f
                    Top = 50.0f, 0.0f
                    Right = 180.0f, 0.5f
                    Bottom = 140.0f, 0.0f
                }
            ProgressMeterLabel = ProgressPieLabel.None
            ProgressMeterColor = Color.FromArgb(100, 220, 220, 220)
            ProgressMeterBackgroundColor = Color.FromArgb(100, 20, 20, 20)
            ProgressMeterUseFont = false
            ProgressMeterFontSpacing = 0.0f
            ProgressMeterColonExtraSpacing = 0.0f
            ProgressMeterPercentExtraSpacing = 0.0f
            ProgressMeterLabelSize = 0.4f

            PacemakerPosition =
                {
                    RelativeToPlayfield = true
                    Left = -128.0f, 0.5f
                    Top = -185.0f, 0.5f
                    Right = 128.0f, 0.5f
                    Bottom = -150.0f, 0.5f
                }

            JudgementCounterEnabled = false
            JudgementCounterPosition =
                {
                    RelativeToPlayfield = false
                    Left = 20.0f, 0.0f
                    Top = -320.0f, 1.0f
                    Right = 120.0f, 0.0f
                    Bottom = -20.0f, 1.0f
                }
            JudgementCounterFadeTime = 200.0f<ms / rate>
            JudgementCounterPopAmount = 0.5f
            JudgementCounterOpacity = 0.5f
            JudgementCounterTextScale = 0.7f
            JudgementCounterUseFont = false
            JudgementCounterFontSpacing = 0.0f
            JudgementCounterDotExtraSpacing = 0.0f
            JudgementCounterColonExtraSpacing = 0.0f
            JudgementCounterShowRatio = false
            JudgementCounterShowLabels = false
            JudgementCounterCustomDisplay = Map.empty

            RateModMeterEnabled = false
            RateModMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = -70.0f, 1.0f
                    Right = 100.0f, 0.5f
                    Bottom = -40.0f, 1.0f
                }
            RateModMeterShowMods = true

            BPMMeterEnabled = false
            BPMMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = -40.0f, 1.0f
                    Right = 100.0f, 0.5f
                    Bottom = -10.0f, 1.0f
                }

            InputMeterEnabled = false
            InputMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = 30.0f, 1.0f
                    Top = -400.0f, 1.0f
                    Right = 200.0f, 1.0f
                    Bottom = -100.0f, 1.0f
                }
            InputMeterScrollSpeed = 0.75f<rate / ms>
            InputMeterKeyColor = Color.FromArgb(127, 255, 255, 255)
            InputMeterKeyFadeTime = 300.0f<ms / rate>
            InputMeterColumnPadding = 0.25f
            InputMeterShowInputs = true
            InputMeterInputColor = Color.FromArgb(127, 255, 255, 255)
            InputMeterInputFadeDistance = 100.0f
            InputMeterScrollDownwards = false
            InputMeterJudgementColors = false
            InputMeterShowKeybinds = false
            InputMeterKeybindColor = Color.FromArgb(127, 0, 0, 0)

            KeysPerSecondMeterEnabled = false
            KeysPerSecondMeterShowAverage = false
            KeysPerSecondMeterShowMax = false
            KeysPerSecondMeterShowTotal = false
            KeysPerSecondMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = 30.0f, 1.0f
                    Top = -100.0f, 1.0f
                    Right = 200.0f, 1.0f
                    Bottom = 0.0f, 1.0f
                }

            MultiplayerScoreTrackerPosition =
                {
                    RelativeToPlayfield = true
                    Left = 50.0f, 1.0f
                    Top = -185.0f, 0.5f
                    Right = 250.0f, 1.0f
                    Bottom = -150.0f, 0.5f
                }

            CustomImageEnabled = false
            CustomImagePosition =
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = 200.0f, 0.0f
                    Right = 100.0f, 0.5f
                    Bottom = 400.0f, 0.0f
                }
            CustomImageFrameTime = 200.0f<ms / rate>
        }

    member this.GetJudgementCounterDisplay(for_ruleset: Ruleset) : int option array =
        match this.JudgementCounterCustomDisplay.TryFind for_ruleset.Judgements.Length with
        | Some xs -> xs
        | None -> Array.create for_ruleset.Judgements.Length None

    member this.GetJudgementMeterDisplay(for_ruleset: Ruleset) : JudgementDisplayType array =
        if this.JudgementMeterUseTexture then
            match this.JudgementMeterCustomDisplay.TryFind for_ruleset.Judgements.Length with
            | Some xs -> xs
            | None -> Array.create for_ruleset.Judgements.Length JudgementDisplayType.Name
        else
            Array.create for_ruleset.Judgements.Length JudgementDisplayType.Name

// todo: song info
// todo: real time clock

type HudTextureRules =
    {
        IsRequired: HudConfig -> bool
        MustBeSquare: HudConfig -> bool
        MaxGridSize: HudConfig -> int * int
    }
    member this.Evaluate(config: HudConfig) : TextureRules =
        {
            IsRequired = this.IsRequired config
            MustBeSquare = this.MustBeSquare config
            MaxGridSize = this.MaxGridSize config
        }

module HudTextureRules =

    let DEFAULT =
        {
            IsRequired = K true
            MustBeSquare = K true
            MaxGridSize = K(16, 32)
        }

    let TEXTURES: Map<string, HudTextureRules> =
        Map.ofList
            [
                "judgements",
                {
                    IsRequired = fun config -> config.JudgementMeterUseTexture
                    MustBeSquare = K false
                    MaxGridSize = K(16, 32)
                }
                "early-late",
                {
                    IsRequired = fun config -> config.EarlyLateMeterUseTexture
                    MustBeSquare = K false
                    MaxGridSize = K(2, 32)
                }
                "skip-button-bg",
                {
                    IsRequired = fun config -> config.SkipButtonBackground.Enable
                    MustBeSquare = K false
                    MaxGridSize = K(1, 1)
                }
                "judgement-counter-judgements",
                {
                    IsRequired = fun config ->
                        config.JudgementCounterShowLabels
                        && config.JudgementCounterCustomDisplay.Values |> Seq.exists (Seq.exists (function Some _ -> true | _ -> false))
                    MustBeSquare = K false
                    MaxGridSize = K(16, 1)
                }
                "judgement-counter-font",
                {
                    IsRequired = fun config -> config.JudgementCounterUseFont
                    MustBeSquare = K false
                    MaxGridSize = K(13, 1)
                }
                "combo-font",
                {
                    IsRequired = fun config -> config.ComboUseFont
                    MustBeSquare = K false
                    MaxGridSize = K(13, 1)
                }
                "accuracy-font",
                {
                    IsRequired = fun config -> config.AccuracyUseFont
                    MustBeSquare = K false
                    MaxGridSize = K(13, 1)
                }
                "progress-meter-font",
                {
                    IsRequired = fun config -> config.ProgressMeterUseFont
                    MustBeSquare = K false
                    MaxGridSize = K(13, 1)
                }
                "custom-image",
                {
                    IsRequired = fun config -> config.CustomImageEnabled
                    MustBeSquare = K false
                    MaxGridSize = K(16, 32)
                }
            ]

    let get (config: HudConfig) (name: string) : TextureRules = TEXTURES.[name].Evaluate config

    let list () : string seq = TEXTURES.Keys :> string seq