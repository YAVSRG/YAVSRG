namespace Prelude.Skinning.Noteskins

open Percyqaz.Data
open Prelude
open Prelude.Gameplay

[<RequireQualifiedAccess>]
type HUDElement =
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
type HUDUserOptions =
    {
        AccuracyEnabled: bool
        AccuracyGradeColors: bool
        AccuracyShowName: bool

        TimingDisplayEnabled: bool
        TimingDisplayFadeTime: float32
        TimingDisplayThickness: float32
        TimingDisplayShowGuide: bool
        TimingDisplayShowNonJudgements: bool
        TimingDisplayReleasesExtraHeight: float32
        TimingDisplayHalfScaleReleases: bool
        TimingDisplayMovingAverageType: TimingDisplayMovingAverageType
        TimingDisplayMovingAverageSensitivity: float32

        ComboEnabled: bool
        ComboLampColors: bool

        SkipButtonEnabled: bool

        JudgementMeterEnabled: bool
        JudgementMeterIgnorePerfect: bool
        JudgementMeterPrioritiseLower: bool

        EarlyLateMeterEnabled: bool

        ProgressMeterEnabled: bool
        ProgressMeterLabel: ProgressMeterLabel

        JudgementCounterEnabled: bool
        JudgementCounterFadeTime: float
        JudgementCounterShowRatio: bool

        RateModMeterEnabled: bool
        RateModMeterShowMods: bool

        BPMMeterEnabled: bool

        InputMeterEnabled: bool
        InputMeterScrollSpeed: float32
    }
    static member Default =
        {
            AccuracyEnabled = true
            AccuracyGradeColors = true
            AccuracyShowName = true

            TimingDisplayEnabled = true
            TimingDisplayFadeTime = 1000.0f
            TimingDisplayThickness = 5.0f
            TimingDisplayShowGuide = true
            TimingDisplayShowNonJudgements = true
            TimingDisplayReleasesExtraHeight = 5.0f
            TimingDisplayHalfScaleReleases = true
            TimingDisplayMovingAverageType = TimingDisplayMovingAverageType.None
            TimingDisplayMovingAverageSensitivity = 0.75f

            ComboEnabled = true
            ComboLampColors = true

            SkipButtonEnabled = true

            JudgementMeterEnabled = true
            JudgementMeterIgnorePerfect = false
            JudgementMeterPrioritiseLower = false

            EarlyLateMeterEnabled = false

            ProgressMeterEnabled = true
            ProgressMeterLabel = ProgressMeterLabel.None

            JudgementCounterEnabled = false
            JudgementCounterFadeTime = 200.0
            JudgementCounterShowRatio = false

            RateModMeterEnabled = false
            RateModMeterShowMods = true

            BPMMeterEnabled = false

            InputMeterEnabled = false
            InputMeterScrollSpeed = 1.5f
        }

[<Json.AutoCodec(false)>]
type HUDPosition =
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
type HUDNoteskinOptions =
    {
        AccuracyPosition: HUDPosition
        AccuracyUseFont: bool
        AccuracyFontSpacing: float32
        AccuracyDotExtraSpacing: float32
        AccuracyPercentExtraSpacing: float32

        TimingDisplayPosition: HUDPosition
        TimingDisplayMovingAverageColor: Color

        ComboPosition: HUDPosition
        ComboGrowth: float32
        ComboPop: float32
        ComboUseFont: bool
        ComboFontSpacing: float32

        SkipButtonPosition: HUDPosition
        SkipButtonBackground: BackgroundTextureOptions

        JudgementMeterPosition: HUDPosition
        JudgementMeterDuration: float32
        JudgementMeterFrameTime: float32
        JudgementMeterUseTexture: bool
        JudgementMeterUseBuiltInAnimation: bool
        JudgementMeterCustomDisplay: Map<int, JudgementDisplayType array>

        EarlyLateMeterPosition: HUDPosition
        EarlyLateMeterDuration: float32
        EarlyLateMeterUseTexture: bool
        EarlyLateMeterFrameTime: float32
        EarlyLateMeterEarlyText: string
        EarlyLateMeterLateText: string
        EarlyLateMeterEarlyColor: Color
        EarlyLateMeterLateColor: Color

        ProgressMeterPosition: HUDPosition
        ProgressMeterColor: Color
        ProgressMeterBackgroundColor: Color
        ProgressMeterLabelSize: float32
        ProgressMeterUseFont: bool
        ProgressMeterFontSpacing: float32
        ProgressMeterColonExtraSpacing: float32
        ProgressMeterPercentExtraSpacing: float32

        PacemakerPosition: HUDPosition

        JudgementCounterPosition: HUDPosition
        JudgementCounterBackground: BackgroundTextureOptions
        JudgementCounterUseFont: bool
        JudgementCounterFontSpacing: float32
        JudgementCounterDotExtraSpacing: float32
        JudgementCounterColonExtraSpacing: float32
        JudgementCounterUseJudgementTextures: bool
        JudgementCounterCustomDisplay: Map<int, int option array>

        RateModMeterPosition: HUDPosition

        BPMMeterPosition: HUDPosition

        InputMeterPosition: HUDPosition
    }
    static member Default =
        {
            AccuracyPosition =
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = 40.0f, 0.0f
                    Right = 100.0f, 0.5f
                    Bottom = 120.0f, 0.0f
                }
            AccuracyUseFont = false
            AccuracyFontSpacing = 0.0f
            AccuracyDotExtraSpacing = 0.0f
            AccuracyPercentExtraSpacing = 0.0f
            
            TimingDisplayPosition =
                {
                    RelativeToPlayfield = true
                    Left = -300.0f, 0.5f
                    Top = 10.0f, 0.5f
                    Right = 300.0f, 0.5f
                    Bottom = 25.0f, 0.5f
                }
            TimingDisplayMovingAverageColor = Color.Aqua

            ComboPosition =
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = -65.0f, 0.5f
                    Right = 100.0f, 0.5f
                    Bottom = -5.0f, 0.5f
                }
            ComboGrowth = 0.01f
            ComboPop = 5.0f
            ComboUseFont = false
            ComboFontSpacing = 0.0f

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

            JudgementMeterPosition = 
                {
                    RelativeToPlayfield = true
                    Left = -128.0f, 0.5f
                    Top =  -160.0f, 0.5f
                    Right = 128.0f, 0.5f
                    Bottom = -105.0f, 0.5f
                }
            JudgementMeterUseTexture = false
            JudgementMeterUseBuiltInAnimation = true
            JudgementMeterDuration = 300.0f
            JudgementMeterFrameTime = 50f
            JudgementMeterCustomDisplay = Map.empty

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

            ProgressMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = 120.0f, 0.5f
                    Top = 50.0f, 0.0f
                    Right = 180.0f, 0.5f
                    Bottom = 140.0f, 0.0f
                }
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

            JudgementCounterPosition = 
                {
                    RelativeToPlayfield = false
                    Left = 20.0f, 0.0f
                    Top = -320.0f, 1.0f
                    Right = 220.0f, 0.0f
                    Bottom = -20.0f, 1.0f
                }
            JudgementCounterBackground =
                {
                    Enable = false
                    Scale = 1.1f
                    AlignmentX = 0.0f
                    AlignmentY = 0.0f
                }
            JudgementCounterUseFont = false
            JudgementCounterFontSpacing = 0.0f
            JudgementCounterDotExtraSpacing = 0.0f
            JudgementCounterColonExtraSpacing = 0.0f
            JudgementCounterUseJudgementTextures = false
            JudgementCounterCustomDisplay = Map.empty

            RateModMeterPosition = 
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = -70.0f, 1.0f
                    Right = 100.0f, 0.5f
                    Bottom = -40.0f, 1.0f
                }
                
            BPMMeterPosition = 
                {
                    RelativeToPlayfield = true
                    Left = -100.0f, 0.5f
                    Top = -40.0f, 1.0f
                    Right = 100.0f, 0.5f
                    Bottom = -10.0f, 1.0f
                }

            InputMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = 20.0f, 1.0f
                    Top = -580.0f, 1.0f
                    Right = 440.0f, 1.0f
                    Bottom = -20.0f, 1.0f
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
// todo: multiplayer player list