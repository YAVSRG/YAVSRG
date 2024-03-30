namespace Prelude.Content.Noteskins

open Percyqaz.Data
open Prelude.Common
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

[<RequireQualifiedAccess>]
[<Json.AutoCodec>]
type JudgementDisplayType =
    | Name
    | Texture of int

[<RequireQualifiedAccess>]
type ProgressMeterLabel =
    | None = 0
    | Countdown = 1
    | Percentage = 2

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

        ComboEnabled: bool
        ComboLampColors: bool

        SkipButtonEnabled: bool

        JudgementMeterEnabled: bool
        JudgementMeterIgnorePerfect: bool
        JudgementMeterPrioritiseLower: bool

        EarlyLateMeterEnabled: bool
        EarlyLateMeterFadeTime: float32

        ProgressMeterEnabled: bool
        ProgressMeterLabel: ProgressMeterLabel

        JudgementCounterEnabled: bool
        JudgementCounterFadeTime: float

        RateModMeterEnabled: bool
        RateModMeterShowMods: bool

        BPMMeterEnabled: bool
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

            ComboEnabled = true
            ComboLampColors = true

            SkipButtonEnabled = true

            JudgementMeterEnabled = false
            JudgementMeterIgnorePerfect = false
            JudgementMeterPrioritiseLower = false

            EarlyLateMeterEnabled = false
            EarlyLateMeterFadeTime = 800.0f

            ProgressMeterEnabled = true
            ProgressMeterLabel = ProgressMeterLabel.None

            JudgementCounterEnabled = false
            JudgementCounterFadeTime = 200.0

            RateModMeterEnabled = false
            RateModMeterShowMods = true

            BPMMeterEnabled = false
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
type HUDNoteskinOptions =
    {
        AccuracyPosition: HUDPosition

        TimingDisplayPosition: HUDPosition

        ComboPosition: HUDPosition
        ComboGrowth: float32
        ComboPop: float32

        SkipButtonPosition: HUDPosition

        JudgementMeterPosition: HUDPosition
        JudgementMeterUseBuiltInAnimation: bool
        JudgementMeterDuration: float32
        JudgementMeterFrameTime: float32
        JudgementMeterUseTexture: bool
        JudgementMeterCustomDisplay: Map<int, JudgementDisplayType array>

        EarlyLateMeterPosition: HUDPosition
        EarlyLateMeterEarlyText: string
        EarlyLateMeterLateText: string
        EarlyLateMeterEarlyColor: Color
        EarlyLateMeterLateColor: Color

        ProgressMeterPosition: HUDPosition
        ProgressMeterColor: Color
        ProgressMeterBackgroundColor: Color

        PacemakerPosition: HUDPosition

        JudgementCounterPosition: HUDPosition

        RateModMeterPosition: HUDPosition

        BPMMeterPosition: HUDPosition
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
            
            TimingDisplayPosition =
                {
                    RelativeToPlayfield = true
                    Left = -300.0f, 0.5f
                    Top = 10.0f, 0.5f
                    Right = 300.0f, 0.5f
                    Bottom = 25.0f, 0.5f
                }

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

            SkipButtonPosition =
                {
                    RelativeToPlayfield = true
                    Left =  -200.0f, 0.5f
                    Top = 130.0f, 0.5f
                    Right = 200.0f, 0.5f
                    Bottom = 230.0f, 0.5f
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
            EarlyLateMeterEarlyText = "Fast"
            EarlyLateMeterLateText = "Slow"
            EarlyLateMeterEarlyColor = Color.FromArgb(52, 79, 235)
            EarlyLateMeterLateColor = Color.FromArgb(235, 52, 52)

            ProgressMeterPosition =
                {
                    RelativeToPlayfield = true
                    Left = 100.0f, 0.5f
                    Top = 50.0f, 0.0f
                    Right = 200.0f, 0.5f
                    Bottom = 110.0f, 0.0f
                }
            ProgressMeterColor = Color.FromArgb(100, 220, 220, 220)
            ProgressMeterBackgroundColor = Color.FromArgb(100, 20, 20, 20)

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
        }
    member this.GetJudgementMeterDisplay(for_ruleset: Ruleset) : JudgementDisplayType array =
        if this.JudgementMeterUseTexture then
            match this.JudgementMeterCustomDisplay.TryFind for_ruleset.Judgements.Length with
            | Some xs -> xs
            | None -> Array.create for_ruleset.Judgements.Length JudgementDisplayType.Name
        else Array.create for_ruleset.Judgements.Length JudgementDisplayType.Name

// todo: song info
// todo: real time clock
// todo: multiplayer player list