namespace Prelude.Content.Noteskins

open Percyqaz.Data
open Prelude.Common

[<RequireQualifiedAccess>]
type ProgressMeterLabel =
    | None = 0
    | Countdown = 1
    | Percentage = 2

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
        JudgementMeterFadeTime: float32
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
            JudgementMeterFadeTime = 800.0f
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
                    RelativeToPlayfield = false
                    Left = -128.0f, 0.5f
                    Top =  -160.0f, 0.5f
                    Right = 128.0f, 0.5f
                    Bottom = -105.0f, 0.5f
                }

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

[<Json.AutoCodec(false)>]
type WidgetPosition =
    {
        Enabled: bool
        Float: bool
        Left: float32
        LeftA: float32
        Top: float32
        TopA: float32
        Right: float32
        RightA: float32
        Bottom: float32
        BottomA: float32
    }
    static member Default =
        {
            Enabled = false
            Float = true
            Left = 0.0f
            LeftA = 0.0f
            Top = 0.0f
            TopA = 0.0f
            Right = 0.0f
            RightA = 1.0f
            Bottom = 0.0f
            BottomA = 1.0f
        }

module HUD =

    [<Json.AutoCodec(false)>]
    type AccuracyMeter =
        {
            Position: WidgetPosition // move to ns
            GradeColors: bool
            ShowName: bool
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
                        Left = -100.0f
                        LeftA = 0.5f
                        Top = 40.0f
                        TopA = 0.0f
                        Right = 100.0f
                        RightA = 0.5f
                        Bottom = 120.0f
                        BottomA = 0.0f
                    }
                GradeColors = true
                ShowName = true
            }

    [<Json.AutoCodec(false)>]
    type HitMeter =
        {
            Position: WidgetPosition // move to ns
            AnimationTime: float32
            Thickness: float32
            ShowGuide: bool
            ShowNonJudgements: bool
            ReleasesExtraHeight: float32
            HalfScaleReleases: bool
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
                        Left = -300.0f
                        LeftA = 0.5f
                        Top = 10.0f
                        TopA = 0.5f
                        Right = 300.0f
                        RightA = 0.5f
                        Bottom = 25.0f
                        BottomA = 0.5f
                    }
                AnimationTime = 1000.0f
                Thickness = 5.0f
                ShowGuide = true
                ShowNonJudgements = true
                ReleasesExtraHeight = 5.0f
                HalfScaleReleases = true
            }

    [<Json.AutoCodec(false)>]
    type Combo =
        {
            Position: WidgetPosition // move to ns
            Growth: float32 // move to ns
            Pop: float32 // move to ns
            LampColors: bool
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
                        Left = -100.0f
                        LeftA = 0.5f
                        Top = -10.0f
                        TopA = 0.45f
                        Right = 100.0f
                        RightA = 0.5f
                        Bottom = 50.0f
                        BottomA = 0.45f
                    }
                Growth = 0.01f
                Pop = 5.0f
                LampColors = true
            }

    [<Json.AutoCodec(false)>]
    type SkipButton =
        {
            Position: WidgetPosition // move to ns
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
                        Left = -200.0f
                        LeftA = 0.5f
                        Top = 20.0f
                        TopA = 0.6f
                        Right = 200.0f
                        RightA = 0.5f
                        Bottom = 120.0f
                        BottomA = 0.6f
                    }
            }

    [<Json.AutoCodec(false)>]
    type JudgementMeter =
        {
            Position: WidgetPosition // move to ns
            AnimationTime: float32
            IgnorePerfectJudgements: bool
            PrioritiseLowerJudgements: bool
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = false
                        Float = false
                        Left = -128.0f
                        LeftA = 0.5f
                        Top = -106.0f
                        TopA = 0.45f
                        Right = 128.0f
                        RightA = 0.5f
                        Bottom = -50.0f
                        BottomA = 0.45f
                    }
                AnimationTime = 800.0f
                IgnorePerfectJudgements = false
                PrioritiseLowerJudgements = false
            }

    [<Json.AutoCodec(false)>]
    type EarlyLateMeter =
        {
            Position: WidgetPosition // move to ns
            AnimationTime: float32
            EarlyText: string // move to ns
            EarlyColor: Color // move to ns
            LateText: string // move to ns
            LateColor: Color // move to ns
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = false
                        Float = false
                        Left = -128.0f
                        LeftA = 0.5f
                        Top = -130.0f
                        TopA = 0.45f
                        Right = 128.0f
                        RightA = 0.5f
                        Bottom = -96.0f
                        BottomA = 0.45f
                    }
                AnimationTime = 800.0f
                EarlyText = "Fast"
                EarlyColor = Color.FromArgb(52, 79, 235)
                LateText = "Slow"
                LateColor = Color.FromArgb(235, 52, 52)
            }

    [<Json.AutoCodec(false)>]
    type ProgressMeter =
        {
            Position: WidgetPosition // move to ns
            Color: Color // move to ns
            BackgroundColor: Color // move to ns
            Label: ProgressMeterLabel
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
                        Left = 100.0f
                        LeftA = 0.5f
                        Top = 50.0f
                        TopA = 0.0f
                        Right = 200.0f
                        RightA = 0.5f
                        Bottom = 110.0f
                        BottomA = 0.0f
                    }
                Color = Color.FromArgb(100, 220, 220, 220)
                BackgroundColor = Color.FromArgb(100, 20, 20, 20)
                Label = ProgressMeterLabel.None
            }

    [<Json.AutoCodec(false)>]
    type Pacemaker =
        {
            Position: WidgetPosition // move to ns
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
                        Left = -300.0f
                        LeftA = 0.5f
                        Top = -10.0f
                        TopA = 0.55f
                        Right = 300.0f
                        RightA = 0.5f
                        Bottom = 50.0f
                        BottomA = 0.55f
                    }
            }

    [<Json.AutoCodec(false)>]
    type JudgementCounts =
        {
            Position: WidgetPosition // move to ns
            AnimationTime: float
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = false
                        Float = true
                        Left = 20.0f
                        LeftA = 0.0f
                        Top = -320.0f
                        TopA = 1.0f
                        Right = 220.0f
                        RightA = 0.0f
                        Bottom = -20.0f
                        BottomA = 1.0f
                    }
                AnimationTime = 200.0
            }

    [<Json.AutoCodec(false)>]
    type RateModMeter =
        {
            Position: WidgetPosition // move to ns
            ShowMods: bool
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = false
                        Float = false
                        Left = -100.0f
                        LeftA = 0.5f
                        Top = -70.0f
                        TopA = 1.0f
                        Right = 100.0f
                        RightA = 0.5f
                        Bottom = -40.0f
                        BottomA = 1.0f
                    }
                ShowMods = true
            }

    [<Json.AutoCodec(false)>]
    type BPMMeter =
        {
            Position: WidgetPosition // move to ns
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = false
                        Float = false
                        Left = -100.0f
                        LeftA = 0.5f
                        Top = -40.0f
                        TopA = 1.0f
                        Right = 100.0f
                        RightA = 0.5f
                        Bottom = -10.0f
                        BottomA = 1.0f
                    }
            }

// todo: song info
// todo: real time clock
// todo: multiplayer player list