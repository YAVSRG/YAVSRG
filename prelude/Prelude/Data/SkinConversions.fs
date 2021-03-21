namespace Prelude.Data

module SkinConversions =

    //https://osu.ppy.sh/wiki/no/Skinning/skin.ini#[mania]
    module osuSkin =
        open FParsec
        open Prelude.Charts.osu

        let parseBool = int >> fun x -> x <> 0

        type General = {
            Name: string
            Author: string
            Version: string
            AnimationFramerate: int
            AllowSliderBallTint: bool
            ComboBurstRandom: bool
            CursorCentre: bool
            CursorExpand: bool
            CursorRotate: bool
            CursorTrailRotate: bool
            CustomComboBurstSounds: int list
            HitCircleOverlayAboveNumber: bool
            LayeredHitSounds: bool
            SliderBallFlip: bool
            SpinnerFadePlayfield: bool
            SpinnerFrequencyModulate: bool
            SpinnerNoBlink: bool
        } with static member Default: General = {
                Name = ""
                Author = ""
                Version = "1.0"
                AnimationFramerate = -1
                AllowSliderBallTint = false
                ComboBurstRandom = false
                CursorCentre = true
                CursorExpand = true
                CursorRotate = true
                CursorTrailRotate = true
                CustomComboBurstSounds = [30; 60; 90; 120; 240; 480] //not default but cant find it
                HitCircleOverlayAboveNumber = true
                LayeredHitSounds = true
                SliderBallFlip = true
                SpinnerFadePlayfield = false
                SpinnerFrequencyModulate = true
                SpinnerNoBlink = false
            }

        let private readGeneral ((title, settings): Header): General = failwith "nyi"

        type RGB = int * int * int
        type RGBA = int * int * int * int

        type Colours = {
            Combo1: RGB option
            Combo2: RGB option
            Combo3: RGB option
            Combo4: RGB option
            Combo5: RGB option
            Combo6: RGB option
            Combo7: RGB option
            Combo8: RGB option
            InputOverlayText: RGB
            MenuGlow: RGB
            SliderBall: RGB
            SliderBorder: RGB
            SliderTrackOverride: RGB option
            SongSelectActiveText: RGB
            SongSelectInactiveText: RGB
            SpinnerBackground: RGB
            StarBreakAdditive: RGB
        } with static member Default: Colours = {
                Combo1 = Some (255, 192, 0)
                Combo2 = Some (0, 202, 0)
                Combo3 = Some (18, 124, 255)
                Combo4 = Some (242, 24, 57)
                Combo5 = None
                Combo6 = None
                Combo7 = None
                Combo8 = None
                InputOverlayText = (0, 0, 0)
                MenuGlow = (0, 78, 155)
                SliderBall = (2, 170, 255)
                SliderBorder = (255, 255, 255)
                SliderTrackOverride = None
                SongSelectActiveText = (0, 0, 0)
                SongSelectInactiveText = (255, 255, 255)
                SpinnerBackground = (100, 100, 100)
                StarBreakAdditive = (255, 182, 193)
            }

        let private readColours ((title, settings): Header): Colours = failwith "nyi"

        type Fonts = {
            HitCirclePrefix: string
            HitCircleOverlap: int
            ScorePrefix: string
            ScoreOverlap: int
            ComboPrefix: string
            ComboOverlap: int
        } with static member Default = {
                HitCirclePrefix = "default"
                HitCircleOverlap = -2
                ScorePrefix = "score"
                ScoreOverlap = -2
                ComboPrefix = "score"
                ComboOverlap = -2
            }

        let private readFonts ((title, settings): Header): Fonts = failwith "nyi"

        type CatchTheBeat = {
            HyperDash: RGB
            HyperDashFruit: RGB
            HyperDashAfterImage: RGB
        } with static member Default = {
                HyperDash = 255,0,0
                HyperDashFruit = 255,0,0
                HyperDashAfterImage = 255,0,0
            }

        let private readCatchTheBeat ((title, settings): Header): CatchTheBeat = failwith "nyi"

        let maniaDefaultTextures k format =
            match k with
            | 1 -> [|"S"|]
            | 2 -> [|"1"; "1"|]
            | 3 -> [|"1"; "S"; "1"|]
            | 4 -> [|"1"; "2"; "2"; "1"|]
            | 5 -> [|"1"; "2"; "S"; "2"; "1"|]
            | 6 -> [|"1"; "2"; "1"; "1"; "2"; "1"|]
            | 7 -> [|"1"; "2"; "1"; "S"; "1"; "2"; "1"|]
            | 8 -> [|"1"; "2"; "1"; "2"; "2"; "1"; "2"; "1"|]
            | 9 -> [|"1"; "2"; "1"; "2"; "S"; "2"; "1"; "2"; "1"|]
            | 10 -> [|"1"; "2"; "1"; "2"; "1"; "1"; "2"; "1"; "2"; "1"|]
            | _ -> failwith "this keymode is not supported"
            |> Array.map (sprintf format)

        type Mania = {
            Keys: int
            ColumnStart: int
            ColumnRight: int
            ColumnSpacing: int list
            ColumnWidth: int list
            ColumnLineWidth: int list
            BarlineHeight: float
            LightingNWidth: int list
            LightingLWidth: int list
            WidthForNoteHeightScale: int option
            HitPosition: int
            LightPosition: int
            ScorePosition: int
            ComboPosition: int
            JudgementLine: bool
            //LightFramePerSecond: unit // nobody knows what this does
            SpecialStyle: int
            ComboBurstStyle: int
            SplitStages: bool option
            StageSeparation: int
            SeparateScore: bool
            KeysUnderNotes: bool
            UpsideDown: bool
            KeyFlipWhenUpsideDown: bool
            KeyFlipWhenUpsideDownN: bool array
            NoteFlipWhenUpsideDown: bool
            KeyFlipWhenUpsideDownND: bool array
            NoteFlipWhenUpsideDownN: bool array
            NoteFlipWhenUpsideDownNH: bool array
            NoteFlipWhenUpsideDownNL: bool array
            NoteFlipWhenUpsideDownNT: bool array
            NoteBodyStyle: int
            NoteBodyStyleN: int array
            ColourN: RGBA array
            ColourLightN: RGB array
            ColourColumnLine: RGBA
            ColourBarline: RGBA
            ColourJudgementLine: RGB
            ColourKeyWarning: RGB
            ColourHold: RGBA
            ColourBreak: RGB
            KeyImageN: string array
            KeyImageND: string array
            NoteImageN: string array
            NoteImageNH: string array
            NoteImageNL: string array
            NoteImageNT: string array
            StageLeft: string
            StageRight: string
            StageBottom: string
            StageHint: string
            StageLight: string
            LightingN: string
            LightingL: string
            WarningArrow: string
            Hit0: string
            Hit50: string
            Hit100: string
            Hit200: string
            Hit300: string
            Hit300g: string
        } with static member Default(k) = {
                Keys = k
                ColumnStart = 136
                ColumnRight = 19
                ColumnSpacing = [0]
                ColumnWidth = [30]
                ColumnLineWidth = [2]
                BarlineHeight = 1.2
                LightingNWidth = []
                LightingLWidth = []
                WidthForNoteHeightScale = None
                HitPosition = 402
                LightPosition = 413
                ScorePosition = 80 //not default but cant find it
                ComboPosition = 180 //^
                JudgementLine = false
                SpecialStyle = 0
                ComboBurstStyle = 1
                SplitStages = None
                StageSeparation = 40
                SeparateScore = true
                KeysUnderNotes = false
                UpsideDown = false
                KeyFlipWhenUpsideDown = true
                KeyFlipWhenUpsideDownN = Array.create k true
                NoteFlipWhenUpsideDown = true
                KeyFlipWhenUpsideDownND = Array.create k true
                NoteFlipWhenUpsideDownN = Array.create k true
                NoteFlipWhenUpsideDownNH = Array.create k true
                NoteFlipWhenUpsideDownNL = Array.create k true
                NoteFlipWhenUpsideDownNT = Array.create k true
                NoteBodyStyle = 1
                NoteBodyStyleN = Array.create k 1
                ColourN = Array.create k (0, 0, 0, 255)
                ColourLightN = Array.create k (55, 255, 255)
                ColourColumnLine = 255, 255, 255, 255
                ColourBarline = 255, 255, 255, 255
                ColourJudgementLine = 255, 255, 255
                ColourKeyWarning = 0, 0, 0
                ColourHold = 255, 191, 51, 255
                ColourBreak = 255, 0, 0
                KeyImageN = maniaDefaultTextures k "mania-key%s"
                KeyImageND = maniaDefaultTextures k "mania-key%sD"
                NoteImageN = maniaDefaultTextures k "mania-note%s"
                NoteImageNH = maniaDefaultTextures k "mania-note%sH"
                NoteImageNL = maniaDefaultTextures k "mania-note%sL"
                NoteImageNT = maniaDefaultTextures k "mania-note%sT"
                StageLeft = "mania-stage-left"
                StageRight = "mania-stage-right"
                StageBottom = "mania-stage-bottom" // ?
                StageHint = "mania-stage-hint"
                StageLight = "mania-stage-light"
                LightingN = "lightingN"
                LightingL = "lightingL"
                WarningArrow = "" // ?
                Hit0 = "mania-hit0"
                Hit50 = "mania-hit50"
                Hit100 = "mania-hit100"
                Hit200 = "mania-hit200"
                Hit300 = "mania-hit300"
                Hit300g = "mania-hit300g"
            }

        let private readMania ((title, settings): Header): Mania = failwith "nyi"

        type SkinData = {
            General: General
            Colours: Colours
            Fonts: Fonts
            Mania: Mania list
        }

        let parseSkinINI =
            tuple5
                (parseHeader "General" .>> spaces)
                (parseHeader "Colours" .>> spaces)
                (parseHeader "Fonts" .>> spaces)
                (attempt (parseHeader "CatchTheBeat" .>> spaces))
                (many (parseHeader "Mania" .>> spaces) .>> eof)
            |>> fun (g, c, f, ctb, ms) -> { General = readGeneral g; Colours = readColours c; Fonts = readFonts f; Mania = List.map readMania ms }
