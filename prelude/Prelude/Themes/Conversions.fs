namespace Prelude.Data

open System.IO
open Prelude.Common

module Conversions =

    //https://osu.ppy.sh/wiki/no/Skinning/skin.ini#[mania]
    module OsuSkin =
        open FParsec
        open Prelude.ChartFormats.``osu!``

        let parseBool = int >> fun x -> x <> 0

        let parseRGB (s: string) : Color =
            match s.Split (",") |> List.ofArray |> List.map int with
            | r :: g :: b :: _ -> Color.FromArgb(r, g, b)
            | _ -> failwith "not enough numbers given for RGB"

        let parseRGBa (s: string) : Color =
            match s.Split (",") |> List.ofArray |> List.map int with
            | r :: g :: b :: a :: _ -> Color.FromArgb(a, r, g, b)
            | r :: g :: b :: _ -> Color.FromArgb(255, r, g, b)
            | _ -> failwith "not enough numbers given for RGBa"

        let parseInts (s: string) =
            match run (spaces >>. (sepBy pint32 (pchar ',' .>> spaces))) s with
            | Success (xs, _, _) -> xs
            | Failure (e, _, _) -> failwith e

        type General =
            { 
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
            }
            static member Default : General =
                {
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
                    CustomComboBurstSounds = [ 30; 60; 90; 120; 240; 480 ] //not default but cant find it
                    HitCircleOverlayAboveNumber = true
                    LayeredHitSounds = true
                    SliderBallFlip = true
                    SpinnerFadePlayfield = false
                    SpinnerFrequencyModulate = true
                    SpinnerNoBlink = false
                }

        let private readGeneral ((title, settings): Header) : General =
            assert (title = "General")

            let f s (key, value) =
                match key with
                | "Name" -> { s with Name = value }
                | "Author" -> { s with Author = value }
                | "Version" -> { s with Version = value }
                | "AnimationFramerate" -> { s with AnimationFramerate = int value }
                | "AllowSliderBallTint" -> { s with AllowSliderBallTint = parseBool value }
                | "ComboBurstRandom" -> { s with ComboBurstRandom = parseBool value }
                | "CursorCentre" -> { s with CursorCentre = parseBool value }
                | "CursorExpand" -> { s with CursorExpand = parseBool value }
                | "CursorRotate" -> { s with CursorRotate = parseBool value }
                | "CursorTrailRotate" -> { s with CursorTrailRotate = parseBool value }
                | "CustomComboBurstSounds" -> { s with CustomComboBurstSounds = parseInts value }
                | "HitCircleOverlayAboveNumer"
                | "HitCircleOverlayAboveNumber" -> { s with HitCircleOverlayAboveNumber = parseBool value }
                | "LayeredHitSounds" -> { s with LayeredHitSounds = parseBool value }
                | "SliderBallFlip" -> { s with SliderBallFlip = parseBool value }
                | "SpinnerFadePlayfield" -> { s with SpinnerFadePlayfield = parseBool value }
                | "SpinnerFrequencyModulate" -> { s with SpinnerFrequencyModulate = parseBool value }
                | "SpinnerNoBlink" -> { s with SpinnerNoBlink = parseBool value }
                | _ -> s

            List.fold f General.Default settings

        type Colours =
            { 
                Combo1: Color option
                Combo2: Color option
                Combo3: Color option
                Combo4: Color option
                Combo5: Color option
                Combo6: Color option
                Combo7: Color option
                Combo8: Color option
                InputOverlayText: Color
                MenuGlow: Color
                SliderBall: Color
                SliderBorder: Color
                SliderTrackOverride: Color option
                SongSelectActiveText: Color
                SongSelectInactiveText: Color
                SpinnerBackground: Color
                StarBreakAdditive: Color
            }
            static member Default : Colours =
                { 
                    Combo1 = Some <| Color.FromArgb(255, 192, 0)
                    Combo2 = Some <| Color.FromArgb(0, 202, 0)
                    Combo3 = Some <| Color.FromArgb(18, 124, 255)
                    Combo4 = Some <| Color.FromArgb(242, 24, 57)
                    Combo5 = None
                    Combo6 = None
                    Combo7 = None
                    Combo8 = None
                    InputOverlayText = Color.Black
                    MenuGlow = Color.FromArgb(0, 78, 155)
                    SliderBall = Color.FromArgb(2, 170, 255)
                    SliderBorder = Color.White
                    SliderTrackOverride = None
                    SongSelectActiveText = Color.Black
                    SongSelectInactiveText = Color.White
                    SpinnerBackground = Color.FromArgb(100, 100, 100)
                    StarBreakAdditive = Color.FromArgb(255, 182, 193)
                }

        let private readColours ((title, settings): Header) : Colours = Colours.Default

        type Fonts =
            { 
                HitCirclePrefix: string
                HitCircleOverlap: int
                ScorePrefix: string
                ScoreOverlap: int
                ComboPrefix: string
                ComboOverlap: int
            }
            static member Default =
                { 
                    HitCirclePrefix = "default"
                    HitCircleOverlap = -2
                    ScorePrefix = "score"
                    ScoreOverlap = -2
                    ComboPrefix = "score"
                    ComboOverlap = -2
                }

        let private readFonts ((title, settings): Header) : Fonts = Fonts.Default

        type CatchTheBeat =
            { 
                HyperDash: Color
                HyperDashFruit: Color
                HyperDashAfterImage: Color
            }
            static member Default =
                { 
                    HyperDash = Color.Red
                    HyperDashFruit = Color.Red
                    HyperDashAfterImage = Color.Red
                }

        let private readCatchTheBeat ((title, settings): Header) : CatchTheBeat = CatchTheBeat.Default

        let maniaDefaultTextures k format =
            match k with
            | 1 -> [| "S" |]
            | 2 -> [| "1"; "1" |]
            | 3 -> [| "1"; "S"; "1" |]
            | 4 -> [| "1"; "2"; "2"; "1" |]
            | 5 -> [| "1"; "2"; "S"; "2"; "1" |]
            | 6 -> [| "1"; "2"; "1"; "1"; "2"; "1" |]
            | 7 -> [| "1"; "2"; "1"; "S"; "1"; "2"; "1" |]
            | 8 -> [| "1"; "2"; "1"; "2"; "2"; "1"; "2"; "1" |]
            | 9 -> [| "1"; "2"; "1"; "2"; "S"; "2"; "1"; "2"; "1" |]
            | 10 -> [| "1"; "2"; "1"; "2"; "1"; "1"; "2"; "1"; "2"; "1" |]
            | _ -> Array.create k "1" //not supported
            |> Array.map (sprintf format)

        type Mania =
            { 
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
                SpecialStyle: string
                ComboBurstStyle: int
                SplitStages: bool option
                StageSeparation: int
                SeparateScore: bool
                KeysUnderNotes: bool
                UpsideDown: bool
                KeyFlipWhenUpsideDown: bool
                KeyFlipWhenUpsideDownΔ: bool array
                NoteFlipWhenUpsideDown: bool
                KeyFlipWhenUpsideDownΔD: bool array
                NoteFlipWhenUpsideDownΔ: bool array
                NoteFlipWhenUpsideDownΔH: bool array
                NoteFlipWhenUpsideDownΔL: bool array
                NoteFlipWhenUpsideDownΔT: bool array
                NoteBodyStyle: int
                NoteBodyStyleΔ: int array
                ColourΔ: Color array
                ColourLightΔ: Color array
                ColourColumnLine: Color
                ColourBarline: Color
                ColourJudgementLine: Color
                ColourKeyWarning: Color
                ColourHold: Color
                ColourBreak: Color
                KeyImageΔ: string array
                KeyImageΔD: string array
                NoteImageΔ: string array
                NoteImageΔH: string array
                NoteImageΔL: string array
                NoteImageΔT: string array
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
            }
            static member Default k =
                { 
                    Keys = k
                    ColumnStart = 136
                    ColumnRight = 19
                    ColumnSpacing = [ 0 ]
                    ColumnWidth = [ 30 ]
                    ColumnLineWidth = [ 2 ]
                    BarlineHeight = 1.2
                    LightingNWidth = []
                    LightingLWidth = []
                    WidthForNoteHeightScale = None
                    HitPosition = 402
                    LightPosition = 413
                    ScorePosition = 80 //not default but cant find it
                    ComboPosition = 180 //^
                    JudgementLine = false
                    SpecialStyle = "0"
                    ComboBurstStyle = 1
                    SplitStages = None
                    StageSeparation = 40
                    SeparateScore = true
                    KeysUnderNotes = false
                    UpsideDown = false
                    KeyFlipWhenUpsideDown = true
                    KeyFlipWhenUpsideDownΔ = Array.create k true
                    NoteFlipWhenUpsideDown = true
                    KeyFlipWhenUpsideDownΔD = Array.create k true
                    NoteFlipWhenUpsideDownΔ = Array.create k true
                    NoteFlipWhenUpsideDownΔH = Array.create k true
                    NoteFlipWhenUpsideDownΔL = Array.create k true
                    NoteFlipWhenUpsideDownΔT = Array.create k true
                    NoteBodyStyle = 1
                    NoteBodyStyleΔ = Array.create k 1
                    ColourΔ = Array.create k Color.Black
                    ColourLightΔ = Array.create k (Color.FromArgb(55, 255, 255))
                    ColourColumnLine = Color.White
                    ColourBarline = Color.White
                    ColourJudgementLine = Color.White
                    ColourKeyWarning = Color.Black
                    ColourHold = Color.FromArgb(255, 191, 51, 255)
                    ColourBreak = Color.Red
                    KeyImageΔ = maniaDefaultTextures k "mania-key%s"
                    KeyImageΔD = maniaDefaultTextures k "mania-key%sD"
                    NoteImageΔ = maniaDefaultTextures k "mania-note%s"
                    NoteImageΔH = maniaDefaultTextures k "mania-note%sH"
                    NoteImageΔL = maniaDefaultTextures k "mania-note%sL"
                    NoteImageΔT = maniaDefaultTextures k "mania-note%sT"
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

        let (|P_0|_|) (keys: int) (pre: string) (suff: string) (target: string) =
            match run (pstring pre >>. (pint32 .>> pstring suff) .>> eof) target with
            | Success (n, _, _) -> if 0 <= n && n < keys then Some n else None
            | Failure (_, _, _) -> None

        let (|P_1|_|) (keys: int) (pre: string) (suff: string) (target: string) =
            match run (pstring pre >>. (pint32 .>> pstring suff) .>> eof) target with
            | Success (n, _, _) -> if 0 < n && n <= keys then Some (n - 1) else None
            | Failure (_, _, _) -> None

        let private readMania ((title, settings): Header) : Mania =
            assert (title = "Mania")

            match settings with
            | ("Keys", keys) :: settings ->
                let keys = int keys

                let f s (key, value: string) =
                    match key with
                    | "ColumnStart" -> { s with ColumnStart = int value }
                    | "ColumnRight" -> { s with ColumnRight = int value }
                    | "ColumnSpacing" -> { s with ColumnSpacing = parseInts value }
                    | "ColumnWidth" -> { s with ColumnWidth = parseInts value }
                    | "ColumnLineWidth" -> { s with ColumnLineWidth = parseInts value }
                    | "BarlineHeight" -> { s with BarlineHeight = float value }
                    | "LightingNWidth" -> { s with LightingNWidth = parseInts value }
                    | "LightingLWidth" -> { s with LightingLWidth = parseInts value }
                    | "WidthForNoteHeightScale" -> { s with WidthForNoteHeightScale = Some (int value) }
                    | "HitPosition" -> { s with HitPosition = int value }
                    | "LightPosition" -> { s with LightPosition = int value }
                    | "ScorePosition" -> { s with ScorePosition = int value }
                    | "ComboPosition" -> { s with ComboPosition = int value }
                    | "JudgementLine" -> { s with JudgementLine = parseBool value }
                    | "SpecialStyle" -> { s with SpecialStyle = value }
                    | "ComboBurstStyle" -> { s with ComboBurstStyle = int value }
                    | "SplitStages" -> { s with SplitStages = Some (parseBool value) }
                    | "StageSeparation" -> { s with StageSeparation = int value }
                    | "SeparateScore" -> { s with SeparateScore = parseBool value }
                    | "KeysUnderNotes" -> { s with KeysUnderNotes = parseBool value }
                    | "UpsideDown" -> { s with UpsideDown = parseBool value }
                    | "KeyFlipWhenUpsideDown" -> { s with KeyFlipWhenUpsideDown = parseBool value }
                    | P_0 keys "KeyFlipWhenUpsideDown" "" n -> s.KeyFlipWhenUpsideDownΔ.[n] <- parseBool value; s
                    | "NoteFlipWhenUpsideDown" -> { s with NoteFlipWhenUpsideDown = parseBool value }
                    | P_0 keys "KeyFlipWhenUpsideDown" "D" n -> s.KeyFlipWhenUpsideDownΔD.[n] <- parseBool value; s
                    | P_0 keys "NoteFlipWhenUpsideDown" "" n -> s.NoteFlipWhenUpsideDownΔ.[n] <- parseBool value; s
                    | P_0 keys "NoteFlipWhenUpsideDown" "H" n -> s.NoteFlipWhenUpsideDownΔH.[n] <- parseBool value; s
                    | P_0 keys "NoteFlipWhenUpsideDown" "L" n -> s.NoteFlipWhenUpsideDownΔL.[n] <- parseBool value; s
                    | P_0 keys "NoteFlipWhenUpsideDown" "T" n -> s.NoteFlipWhenUpsideDownΔT.[n] <- parseBool value; s
                    | "NoteBodyStyle" -> { s with NoteBodyStyle = int value }
                    | P_0 keys "NoteBodyStyle" "" n -> s.NoteBodyStyleΔ.[n] <- int value; s
                    | P_1 keys "Colour" "" n -> s.ColourΔ.[n] <- parseRGBa value; s
                    | P_1 keys "ColourLight" "" n -> s.ColourLightΔ.[n] <- parseRGB value; s
                    | "ColourColumnLine" -> { s with ColourColumnLine = parseRGBa value }
                    | "ColourBarline" -> { s with ColourBarline = parseRGBa value }
                    | "ColourJudgementLine" -> { s with ColourJudgementLine = parseRGB value }
                    | "ColourKeyWarning" -> { s with ColourKeyWarning = parseRGB value }
                    | "ColourHold" -> { s with ColourHold = parseRGBa value }
                    | "ColourBreak" -> { s with ColourBreak = parseRGB value }
                    | P_0 keys "KeyImage" "" n -> s.KeyImageΔ.[n] <- value; s
                    | P_0 keys "KeyImage" "D" n -> s.KeyImageΔD.[n] <- value; s
                    | P_0 keys "NoteImage" "" n -> s.NoteImageΔ.[n] <- value; s
                    | P_0 keys "NoteImage" "H" n -> s.NoteImageΔH.[n] <- value; s
                    | P_0 keys "NoteImage" "L" n -> s.NoteImageΔL.[n] <- value; s
                    | P_0 keys "NoteImage" "T" n -> s.NoteImageΔT.[n] <- value; s
                    | "StageLeft" -> { s with StageLeft = value }
                    | "StageRight" -> { s with StageRight = value }
                    | "StageBottom" -> { s with StageBottom = value }
                    | "StageHint" -> { s with StageHint = value }
                    | "StageLight" -> { s with StageLight = value }
                    | "LightingN" -> { s with LightingN = value }
                    | "LightingL" -> { s with LightingL = value }
                    | "WarningArrow" -> { s with WarningArrow = value }
                    | "Hit0" -> { s with Hit0 = value }
                    | "Hit50" -> { s with Hit50 = value }
                    | "Hit100" -> { s with Hit100 = value }
                    | "Hit200" -> { s with Hit200 = value }
                    | "Hit300" -> { s with Hit300 = value }
                    | "Hit300g" -> { s with Hit300g = value }
                    | _ -> s

                List.fold f (Mania.Default keys) settings
            | _ -> failwith "mania block did not specify keycount"

        type SkinData = { General: General; Colours: Colours; Fonts: Fonts; Mania: Mania list }

        let skinIniParser =
            tuple5
                (parseHeader "General" .>> spaces)
                (opt (parseHeader "Colours" .>> spaces))
                (opt (parseHeader "Fonts" .>> spaces))
                (opt (parseHeader "CatchTheBeat" .>> spaces))
                (many (parseHeader "Mania" .>> spaces))
            |>> fun (g, c, f, ctb, ms) ->
                    { 
                        General = readGeneral g
                        Colours =
                            c
                            |> Option.map readColours
                            |> Option.defaultValue Colours.Default
                        Fonts =
                            f
                            |> Option.map readFonts
                            |> Option.defaultValue Fonts.Default
                        Mania = List.map readMania ms
                    }

        let parseSkinINI file =
            match runParserOnFile skinIniParser () file System.Text.Encoding.UTF8 with
            | Success (s, _, _) -> s
            | Failure (e, _, _) -> failwith e

        (*
        //constructor can throw an exception!
        type Converter(path) = 
            let data = parseSkinINI (Path.Combine (path, "skin.ini"))

            member this.FindTexture (filename: string) : string list =
                let file = Path.Combine(path, filename)
                if File.Exists(file + "@2x.png") then [file + "@2x.png"]
                elif File.Exists(file + ".png") then [file + ".png"]
                else
                    let rec f i =
                        if File.Exists(file + "-" + i.ToString() + "@2x.png") then
                            file + "-" + i.ToString() + "@2x.png" :: f (i + 1)
                        elif File.Exists(file + "-" + i.ToString() + ".png") then
                            file + "-" + i.ToString() + ".png" :: f (i + 1)
                        else []
                    let result = f 0
                    if result.IsEmpty then failwithf "could not find texture in skin for %A" filename
                    result

            member this.StitchTextures (lnbody: bool) (input: Image list list) : Bitmap * Themes.TextureConfig =
                let rows = input.Length
                let columns = input |> List.map List.length |> List.max

                let width = input.Head.Head.Width
                let height = 
                    if lnbody then
                        if input.Head.Head.Height / width > 10 then Logging.Warn "It looks like you're using the 'Percy LN trick' - LNs won't look right but it can be fixed in config afterwards"
                        width
                    else input.Head.Head.Height
                let sq = max width height

                if width = 0 || height = 0 then failwith "images cannot be 0x0!"
                
                if not (List.forall (List.forall (fun (i: Image) -> i.Width = width && i.Height = height)) input) then
                    Logging.Warn "all images should be the same dimension"

                let bitmap = new Bitmap(sq * columns, sq * rows)
                use g = Graphics.FromImage bitmap

                let mutable y = 0
                for row in input do
                    let mutable x = 0
                    for j in 0 .. (columns - 1) do
                        g.DrawImage
                            (
                                row.[j % row.Length],
                                if width <= height then
                                    Rectangle(x + (height - width) / 2, y, width, height)
                                else
                                    Rectangle(x, y + (width - height) / 2, width, height)
                            )
                        x <- x + sq
                    y <- y + sq
                (bitmap, { Rows = rows; Columns = columns; Tiling = true })

            member this.GrayscaleImage (alphaMult: float32) (bitmap: Image) =
                let bitmap = bitmap :?> Bitmap
                let newBmp = new Bitmap(bitmap.Width, bitmap.Height)
                for x = 0 to bitmap.Width - 1 do
                    for y = 0 to bitmap.Height - 1 do
                        let col = bitmap.GetPixel(x, y)
                        let level = ((float32 col.R * 0.3f) + (float32 col.G + 0.59f) + (float32 col.B * 0.11f)) |> int
                        let level = min 255 level // how is this even possible
                        let nc = Color.FromArgb(int (float32 col.A * alphaMult), level, level, level)
                        newBmp.SetPixel(x, y, nc)
                newBmp :> Image

            member this.ToNoteskin (targetPath: string) (keys: int) =
                Logging.Info "===== Beginning osu -> Interlude skin conversion ====="
                Logging.Info (path + "->" + targetPath)

                if Directory.Exists targetPath then failwith "a folder with this name already exists!"
                Directory.CreateDirectory targetPath |> ignore

                let mania = List.tryFind (fun m -> m.Keys = keys) data.Mania |> Option.defaultValue (Mania.Default keys)

                let skinJson : Themes.NoteskinConfig =
                    { Themes.NoteskinConfig.Default with
                        Name = Path.GetFileName path
                        Author = data.General.Author
                        UseHoldTailTexture = true
                        FlipHoldTail = mania.NoteFlipWhenUpsideDownΔT.[0]
                        PlayfieldColor = mania.ColourΔ.[0]
                        ColumnWidth = 1080f / 512f * (mania.ColumnWidth |> List.head |> float32)
                    }
                JSON.ToFile (Path.Combine(targetPath, "noteskin.json"), true) skinJson
                Logging.Info "Written noteskin config"

                let textures =
                    (fun i -> (mania.NoteImageΔ.[i], mania.NoteImageΔH.[i], mania.NoteImageΔL.[i], mania.NoteImageΔT.[i]))
                    |> List.init keys
                    |> List.distinct

                printfn "%A" textures

                let writer filename (bmp: Bitmap, config) =
                    JSON.ToFile (Path.Combine(targetPath, filename + ".json"), true) config
                    bmp.Save(Path.Combine(targetPath, filename + ".png"))
                
                Logging.Info "Stitching note.png ..."
                textures
                |> List.map (fun (t, _, _, _) -> t)
                |> List.map this.FindTexture
                |> List.map (List.map Bitmap.FromFile)
                |> this.StitchTextures false
                |> writer "note"

                Logging.Info "Generating receptor.png ..."
                mania.NoteImageΔ.[2]
                |> this.FindTexture
                |> List.head
                |> Bitmap.FromFile
                |> (fun b -> [[this.GrayscaleImage 0.5f b]; [this.GrayscaleImage 1.0f b]])
                |> this.StitchTextures false
                |> writer "receptor"

                Logging.Info "Blank noteexplosion.png ..."
                [[new Bitmap(1, 1) :> Image]]
                |> this.StitchTextures false
                |> writer "noteexplosion"

                Logging.Info "Blank holdexplosion.png ..."
                [[new Bitmap(1, 1) :> Image]]
                |> this.StitchTextures false
                |> writer "holdexplosion"
                
                Logging.Info "Stitching holdhead.png ..."
                textures
                |> List.map (fun (_, t, _, _) -> t)
                |> List.map this.FindTexture
                |> List.map (List.map Bitmap.FromFile)
                |> this.StitchTextures false
                |> writer "holdhead"
                
                Logging.Info "Stitching holdbody.png ..."
                textures
                |> List.map (fun (_, _, t, _) -> t)
                |> List.map this.FindTexture
                |> List.map (List.map Bitmap.FromFile)
                |> this.StitchTextures true
                |> writer "holdbody"
                
                Logging.Info "Stitching holdtail.png ..."
                try
                    textures
                    |> List.map (fun (_, _, _, t) -> t)
                    |> List.map this.FindTexture
                with err ->
                    Logging.Info ("Defaulting to head textures because tail textures were missing", err)
                    textures
                    |> List.map (fun (_, t, _, _) -> t)
                    |> List.map this.FindTexture
                |> List.map (List.map Bitmap.FromFile)
                |> this.StitchTextures false
                |> writer "holdtail"

                Logging.Info "Stitching judgements.png ..."
                [mania.Hit300g; mania.Hit300; mania.Hit200; mania.Hit100; mania.Hit50; mania.Hit0]
                |> List.map (fun s -> if s = "" then [new Bitmap(1, 1) :> Image] else this.FindTexture s |> List.truncate 1 |> List.map Bitmap.FromFile)
                |> fun l -> let b = List.head (List.head l) in [new Bitmap(b.Width, b.Height) :> Image] :: l //ridiculous judgement placeholder
                |> this.StitchTextures false
                //|> writer "judgements"
                |> ignore

                Logging.Info "===== Complete! ====="
            *)
    
    let (|OsuSkinArchive|OsuSkinFolder|InterludeSkinArchive|Unknown|) (path: string) =
        if Directory.Exists path then
            if File.Exists (Path.Combine(path, "skin.ini")) then OsuSkinFolder else Unknown
        else
            let s = Path.GetExtension(path).ToLower()
            match s with
            | ".isk" -> InterludeSkinArchive
            | ".osk" -> OsuSkinArchive
            | _ -> Unknown
