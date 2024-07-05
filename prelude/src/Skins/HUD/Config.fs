namespace Prelude.Skins.HudLayouts

open Percyqaz.Common
open Percyqaz.Data
open Prelude
open Prelude.Gameplay
open Prelude.Skins

[<RequireQualifiedAccess>]
type HudElement =
    | Accuracy
    | TimingDisplay
    | Combo
    | SkipButton
    | JudgementMeter
    | EarlyLateMeter
    | ProgressMeter
    | JudgementCounter
    | RateModMeter
    | BPMMeter
    | Pacemaker
    | InputMeter
    static member FULL_LIST = 
        [
            Accuracy
            TimingDisplay
            Combo
            SkipButton
            JudgementMeter
            EarlyLateMeter
            ProgressMeter
            JudgementCounter
            RateModMeter
            BPMMeter
            Pacemaker
            InputMeter
        ]

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type JudgementDisplayType =
    | Name
    | Texture of int

type ProgressMeterLabel =
    | None = 0
    | Countdown = 1
    | Percentage = 2

type TimingDisplayMovingAverageType =
    | None = 0
    | Arrow = 1
    | ReplaceBars = 2

[<Json.AutoCodec(false)>]
type HudPosition =
    {
        RelativeToPlayfield: bool
        Left: float32 * float32
        Top: float32 * float32
        Right: float32 * float32
        Bottom: float32 * float32
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
        TimingDisplayFadeTime: float32
        TimingDisplayThickness: float32
        TimingDisplayShowGuide: bool
        TimingDisplayGuideThickness: float32
        TimingDisplayShowNonJudgements: bool
        TimingDisplayReleasesExtraHeight: float32
        TimingDisplayHalfScaleReleases: bool
        TimingDisplayMovingAverageType: TimingDisplayMovingAverageType
        TimingDisplayMovingAverageSensitivity: float32
        TimingDisplayMovingAverageColor: Color

        ComboEnabled: bool
        ComboPosition: HudPosition
        ComboLampColors: bool
        ComboGrowth: float32
        ComboPop: float32
        ComboUseFont: bool
        ComboFontSpacing: float32

        SkipButtonEnabled: bool
        SkipButtonPosition: HudPosition
        SkipButtonBackground: BackgroundTextureOptions

        JudgementMeterEnabled: bool
        JudgementMeterPosition: HudPosition
        JudgementMeterIgnorePerfect: bool
        JudgementMeterPrioritiseLower: bool
        JudgementMeterDuration: float32
        JudgementMeterFrameTime: float32
        JudgementMeterUseTexture: bool
        JudgementMeterUseBuiltInAnimation: bool
        JudgementMeterCustomDisplay: Map<int, JudgementDisplayType array>

        EarlyLateMeterEnabled: bool
        EarlyLateMeterPosition: HudPosition
        EarlyLateMeterDuration: float32
        EarlyLateMeterUseTexture: bool
        EarlyLateMeterFrameTime: float32
        EarlyLateMeterEarlyText: string
        EarlyLateMeterLateText: string
        EarlyLateMeterEarlyColor: Color
        EarlyLateMeterLateColor: Color

        ProgressMeterEnabled: bool
        ProgressMeterPosition: HudPosition
        ProgressMeterLabel: ProgressMeterLabel
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
        JudgementCounterFadeTime: float
        JudgementCounterShowRatio: bool
        JudgementCounterBackground: BackgroundTextureOptions
        JudgementCounterUseFont: bool
        JudgementCounterFontSpacing: float32
        JudgementCounterDotExtraSpacing: float32
        JudgementCounterColonExtraSpacing: float32
        JudgementCounterUseJudgementTextures: bool
        JudgementCounterCustomDisplay: Map<int, int option array>

        RateModMeterEnabled: bool
        RateModMeterPosition: HudPosition
        RateModMeterShowMods: bool

        BPMMeterEnabled: bool
        BPMMeterPosition: HudPosition

        InputMeterEnabled: bool
        InputMeterPosition: HudPosition
        InputMeterScrollSpeed: float32
        InputMeterKeyColor: Color
        InputMeterKeyFadeTime: float32
        InputMeterShowInputs: bool
        InputMeterInputColor: Color
        InputMeterInputFadeDistance: float32
        InputMeterScrollDownwards: bool

        MultiplayerScoreTrackerPosition: HudPosition
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
            TimingDisplayFadeTime = 1000.0f
            TimingDisplayThickness = 5.0f
            TimingDisplayShowGuide = true
            TimingDisplayGuideThickness = 1.0f
            TimingDisplayShowNonJudgements = true
            TimingDisplayReleasesExtraHeight = 5.0f
            TimingDisplayHalfScaleReleases = true
            TimingDisplayMovingAverageType = TimingDisplayMovingAverageType.None
            TimingDisplayMovingAverageSensitivity = 0.75f
            TimingDisplayMovingAverageColor = Color.Aqua

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

            SkipButtonEnabled = true
            SkipButtonPosition =
                {
                    RelativeToPlayfield = true
                    Left =  -200.0f, 0.5f
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
                    Top =  -160.0f, 0.5f
                    Right = 128.0f, 0.5f
                    Bottom = -105.0f, 0.5f
                }
            JudgementMeterIgnorePerfect = false
            JudgementMeterPrioritiseLower = false
            JudgementMeterUseTexture = false
            JudgementMeterUseBuiltInAnimation = true
            JudgementMeterDuration = 300.0f
            JudgementMeterFrameTime = 50f
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
            EarlyLateMeterDuration = 200.0f
            EarlyLateMeterUseTexture = false
            EarlyLateMeterFrameTime = 20.0f
            EarlyLateMeterEarlyText = "Fast"
            EarlyLateMeterLateText = "Slow"
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
            ProgressMeterLabel = ProgressMeterLabel.None
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
                    Right = 220.0f, 0.0f
                    Bottom = -20.0f, 1.0f
                }
            JudgementCounterFadeTime = 200.0
            JudgementCounterBackground =
                {
                    Enable = false
                    Scale = 1.1f
                    AlignmentX = 0.0f
                    AlignmentY = 0.0f
                }
            JudgementCounterShowRatio = false
            JudgementCounterUseFont = false
            JudgementCounterFontSpacing = 0.0f
            JudgementCounterDotExtraSpacing = 0.0f
            JudgementCounterColonExtraSpacing = 0.0f
            JudgementCounterUseJudgementTextures = false
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
                    Left = 20.0f, 1.0f
                    Top = -580.0f, 1.0f
                    Right = 440.0f, 1.0f
                    Bottom = -20.0f, 1.0f
                }
            InputMeterScrollSpeed = 1.5f
            InputMeterKeyColor = Color.FromArgb(127, 255, 255, 255)
            InputMeterKeyFadeTime = 300.0f
            InputMeterShowInputs = true
            InputMeterInputColor = Color.FromArgb(127, 255, 255, 255)
            InputMeterInputFadeDistance = 100.0f
            InputMeterScrollDownwards = false

            MultiplayerScoreTrackerPosition =
                {
                    RelativeToPlayfield = true
                    Left = 50.0f, 1.0f
                    Top = -185.0f, 0.5f
                    Right = 250.0f, 1.0f
                    Bottom = -150.0f, 0.5f
                }
        }

    member this.GetJudgementCounterDisplay(for_ruleset: Ruleset) : int option array =
        if this.JudgementCounterUseJudgementTextures then
            match this.JudgementCounterCustomDisplay.TryFind for_ruleset.Judgements.Length with
            | Some xs -> xs
            | None -> Array.create for_ruleset.Judgements.Length None
        else Array.create for_ruleset.Judgements.Length None

    member this.GetJudgementMeterDisplay(for_ruleset: Ruleset) : JudgementDisplayType array =
        if this.JudgementMeterUseTexture then
            match this.JudgementMeterCustomDisplay.TryFind for_ruleset.Judgements.Length with
            | Some xs -> xs
            | None -> Array.create for_ruleset.Judgements.Length JudgementDisplayType.Name
        else Array.create for_ruleset.Judgements.Length JudgementDisplayType.Name

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
                "judgement-counter-bg",
                {
                    IsRequired = fun config -> config.JudgementCounterBackground.Enable
                    MustBeSquare = K false
                    MaxGridSize = K(1, 1)
                }
                "skip-button-bg",
                {
                    IsRequired = fun config -> config.SkipButtonBackground.Enable
                    MustBeSquare = K false
                    MaxGridSize = K(1, 1)
                }
                "judgement-counter-judgements",
                {
                    IsRequired = fun config -> config.JudgementCounterUseJudgementTextures
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
            ]

    let get (config: HudConfig) (name: string) = TEXTURES.[name].Evaluate config

    let list () : string seq = TEXTURES.Keys :> string seq
