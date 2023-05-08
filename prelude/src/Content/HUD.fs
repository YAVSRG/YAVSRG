namespace Prelude.Data.Content

open Percyqaz.Json
open Prelude.Common

[<Json.AutoCodec(false)>]
type WidgetPosition =
    {
        Enabled: bool; Float: bool
        Left: float32; LeftA: float32
        Top: float32; TopA: float32
        Right: float32; RightA: float32
        Bottom: float32; BottomA: float32
    }
    static member Default =
        {
            Enabled = false; Float = true
            Left = 0.0f; LeftA = 0.0f
            Top = 0.0f; TopA = 0.0f
            Right = 0.0f; RightA = 1.0f
            Bottom = 0.0f; BottomA = 1.0f
        }

module HUD =

    [<Json.AutoCodec(false)>]
    type AccuracyMeter = 
        { 
            Position: WidgetPosition
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
            Position: WidgetPosition
            AnimationTime: float32
            Thickness: float32
            ShowGuide: bool
            ShowNonJudgements: bool
            ReleasesExtraHeight: float32
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
            }

    [<Json.AutoCodec(false)>]
    type LifeMeter =
        {
            Position: WidgetPosition
            Horizontal: bool
            TipColor: Color
            FullColor: Color
            EmptyColor: Color
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = false
                        Float = false
                        Left = 0.0f
                        LeftA = 1.0f
                        Top = 0.0f
                        TopA = 0.6f
                        Right = 20.0f
                        RightA = 1.0f
                        Bottom = 0.0f
                        BottomA = 1.0f
                    }
                Horizontal = false
                TipColor = Color.FromArgb(100, Color.Red)
                FullColor = Color.White
                EmptyColor = Color.Red
            }

    [<Json.AutoCodec(false)>]
    type Combo =
        {
            Position: WidgetPosition
            Growth: float32
            Pop: float32
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
        { Position: WidgetPosition }
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
            Position: WidgetPosition
            AnimationTime: float32
        }
        static member Default = 
            {
                Position = 
                    {
                        Enabled = false
                        Float = false
                        Left = -128.0f
                        LeftA = 0.5f
                        Top = 30.0f
                        TopA = 0.5f
                        Right = 128.0f
                        RightA = 0.5f
                        Bottom = 86.0f
                        BottomA = 0.5f
                    }
                AnimationTime = 800.0f
            }

    [<Json.AutoCodec(false)>]
    type ProgressMeter =
        { 
            Position: WidgetPosition
            BarHeight: float32
            BarColor: Color
            GlowSize: float32
            GlowColor: Color
        }
        static member Default =
            {
                Position =
                    {
                        Enabled = true
                        Float = false
                        Left = -15.0f
                        LeftA = 0.0f
                        Top = 10.0f
                        TopA = 0.0f
                        Right = -5.0f
                        RightA = 0.0f
                        Bottom = -10.0f
                        BottomA = 1.0f
                    }
                BarHeight = 15.0f
                BarColor = Color.FromArgb(180, 0, 170, 255)
                GlowSize = 2.0f
                GlowColor = Color.FromArgb(100, 80, 190, 255)
            }

    [<Json.AutoCodec(false)>]
    type Pacemaker =
        { Position: WidgetPosition }
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
            Position: WidgetPosition
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

    //song info
    //mod info
    //clock