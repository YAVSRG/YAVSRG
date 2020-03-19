module Prelude.Charts.osu

open FParsec

//https://osu.ppy.sh/help/wiki/osu!_File_Formats/Osu_(file_format)
//https://osu.ppy.sh/community/forums/topics/1869?start=12468#p12468
(*
  Not currently supported:
    Combo colors/starters
    Parsing of storyboard events
*)

(*
    Basic .osu and .osb file parsing tools to read it into a data structure
*)

type Offset = float

type Header = string * (string * string) list

let formatHeader (title, entries) =
    let f (key, value) = key + ":" + value
    "[" + title + "]\n" + (String.concat "\n" (List.map f entries))

type TimingEffect =
    | Kiai = 1
    | OmitFirstBarline = 8

type SampleSet =
    | None = 0 //todo: review 
    | Default = 0
    | Normal = 1
    | Soft = 2
    | Drum = 3

type TimingPoint =
    | BPM of Offset * float * int * (SampleSet * int * int) * TimingEffect
    | SV of Offset * float * (SampleSet * int * int) * TimingEffect
    override this.ToString() =
        match this with
        | BPM (time, msPerBeat, meter, (set,index,volume), effect) ->
            String.concat "," [string time; string msPerBeat; string meter; set |> int |> string; string index; string volume; "1"; effect |> int |> string]
        | SV (time, value, (set,index,volume), effect) ->
            String.concat "," [string time; string (-100.0/value); "4"; set |> int |> string; string index; string volume; "0"; effect |> int |> string]

type Point = float * float

type SliderShape =
    | Linear
    | Catmull
    | Bezier
    | PerfectCircle
    override this.ToString() =
        match this with
        | Linear -> "L" | Catmull -> "C" | Bezier -> "B" | PerfectCircle -> "P"

type HitAddition = SampleSet * SampleSet * int * int * string
let formatHitAddition (s1,s2,index,volume,file) = String.concat ":" [s1 |> int |> string; s2 |> int |> string; string index; string volume; file]

type HitSound =
    | Normal = 1
    | Whistle = 2
    | Finish = 4
    | Clap = 8

type HitObject =
    | HitCircle of Point * Offset * HitSound * HitAddition
    | HoldNote of Point * Offset * Offset * HitSound * HitAddition
    | Slider of Point * Offset * int * (SliderShape * Point list) * float * HitSound * HitSound list * (SampleSet * SampleSet) list * HitAddition
    | Spinner of Offset * Offset * HitSound * HitAddition
    override this.ToString() = 
        match this with
        | HitCircle ((x,y),offset,hs,addition) -> String.concat "," [x |> string; y |> string; offset |> string; "1"; hs |> int |> string; formatHitAddition addition]
        | HoldNote ((x,y),start,finish,hs,addition) -> String.concat "," [string x; string y; string start; "128"; hs |> int |> string; (string finish) + ":" + formatHitAddition addition]
        | Slider ((x,y),offset,repeats,points,length,hs,phss,padditions,addition) -> "nyi"
        | Spinner (start,finish,hs,addition) -> String.concat "," ["320"; "240"; string start; "8"; hs |> int |> string; string finish; formatHitAddition addition]
        

let private comma = pchar ','
let private colon = pchar ':'
let private pipe = pchar '|'
let private parseNum = pfloat
let private parseInt = pint64 |>> int
let private parseName = (many1Satisfy isLetter)
let private parseQuote =
    between (pchar '"') (pchar '"') (many1Satisfy (fun c -> c <> '"')) <|> (many1Satisfy (Text.IsWhitespace >> not))

let private parseKeyValue = parseName .>> spaces .>> colon .>> spaces .>>. (restOfLine true)
let private parseHeaderTitle = between (pchar '[') (pchar ']') parseName
let parseHeader: Parser<Header, unit> = parseHeaderTitle .>> newline .>>. many parseKeyValue |>> Header

let parseTimingPoint: Parser<TimingPoint, unit> =
    (tuple4 (parseNum .>> comma) (parseNum .>> comma) (parseInt .>> comma) (parseInt .>> comma))
    .>>. (tuple4 (parseInt .>> comma) (parseInt .>> comma) (parseInt .>> comma) parseInt) |>>
    fun ((offset, value, meter, sampleSet), (sampleIndex, volume, isBpm, effects)) ->
        if isBpm > 0
        then BPM(offset, value, meter, (enum sampleSet, sampleIndex, volume), enum effects)
        else SV(offset, -100.0 / value, (enum sampleSet, sampleIndex, volume), enum effects)

let parseTimingPoints = pstring "[TimingPoints]" >>. newline >>. many (parseTimingPoint .>> newline)

let private parsePoint = parseNum .>> comma .>>. parseNum |>> Point
let private parsePointColon = parseNum .>> colon .>>. parseNum |>> Point
let private parseAddition =
    tuple5 (parseInt .>> colon) (parseInt .>> colon) (parseInt .>> colon) (parseInt .>> colon) (restOfLine true)
    |>> fun (normal, addition, index, volume, file) -> (enum normal, enum addition, index, volume, file)

let parseSliderType =
    pchar 'L' <|> pchar 'B' <|> pchar 'C' <|> pchar 'P' |>> fun c ->
        match c with
        | 'L' -> Linear
        | 'B' -> Bezier
        | 'C' -> Catmull
        | 'P' -> PerfectCircle
        | _ -> failwith "Unknown slider shape"

let parseSliderSounds =
    (sepBy1 (parseInt |>> enum) pipe) .>> comma
    .>>. (sepBy1 ((parseInt |>> enum) .>> colon .>>. (parseInt |>> enum)) pipe)
let sliderPoints = parseSliderType .>> pipe .>>. sepBy1 parsePointColon pipe

let parseHitObject: Parser<HitObject, unit> =
    tuple4 (parsePoint .>> comma) (parseNum .>> comma) (parseInt .>> comma) ((parseInt .>> comma) |>> enum)
    >>= (fun (pos, offset, objType, hitsound) ->
        match objType &&& 139 with
        | 1 -> parseAddition |>> fun addition -> HitCircle(pos, offset, hitsound, addition)
        | 2 ->
            tuple5 (sliderPoints .>> comma) (parseInt .>> comma) (parseNum .>> comma) (parseSliderSounds .>> comma)
                parseAddition
            |>> fun (points, slides, length, (sounds, sampleSets), addition) ->
                Slider(pos, offset, slides, points, length, hitsound, sounds, sampleSets, addition)
        | 8 ->
            pipe2 (parseNum .>> comma) parseAddition
                (fun endTime addition -> Spinner(offset, endTime, hitsound, addition))
        | 128 ->
            pipe2 (parseNum .>> colon) parseAddition
                (fun endTime addition -> HoldNote(pos, offset, endTime, hitsound, addition))
        | _ -> failwith "Unknown hitobject type")

let parseHitObjects = pstring "[HitObjects]" >>. newline >>. many parseHitObject

(*
    Storyboard parsing code (WIP)
*)

type Easing =
    | None = 0
    | Decelerate = 1
    | Accelerate = 2

type Layer =
    | Background = 0
    | Fail = 1
    | Pass = 2
    | Foreground = 3
    | Overlay = 4

type LoopType =
    | LoopForever = 0
    | LoopOnce = 1

type TriggerType =
    | HitSoundClap
    | HitSoundFinish
    | HitSoundWhistle
    | Passing
    | Failing

type SpriteOrigin =
    | TopLeft = 0
    | TopCentre = 1
    | TopRight = 2
    | CentreLeft = 3
    | Centre = 4
    | CentreRight = 5
    | BottomLeft = 6
    | BottomCentre = 7
    | BottomRight = 8

type SpriteParameter =
    | VerticalFlip
    | HorizontalFlip
    | AdditiveBlendColor

type StoryboardEvent =
    | Fade of Offset * Offset * Easing * float * float
    | Move of Offset * Offset * Easing * Point * Point
    | Move_X of Offset * Offset * Easing * float * float
    | Move_Y of Offset * Offset * Easing * float * float
    | Scale of Offset * Offset * Easing * float
    | Vector_Scale of Offset * Offset * Easing * Point
    | Rotate of Offset * Offset * Easing * float * float
    | Color of Offset * Offset * Easing * (int * int * int) * (int * int * int)
    | Loop of Offset * int * StoryboardEvent list
    | Trigger_Loop of Offset * Offset * TriggerType * StoryboardEvent list
    | Parameter of Offset * Offset * SpriteParameter

type StoryboardObject =
    | Sprite of Layer * SpriteOrigin * string * Point * StoryboardEvent list
    | Animation of Layer * SpriteOrigin * string * Point * int * Offset * LoopType * StoryboardEvent list
    | Sample of Offset * Layer * string * int
    | Break of Offset * Offset
    | Background of string * Point
    | Video of Offset * string * Point
    override this.ToString() = 
        match this with
        | Background (filename, (x,y)) -> String.concat "," ["0"; "0"; "\""+filename+"\""; string x; string y]
        | Break (start, finish) -> String.concat "," ["2"; string start; string finish]
        | _ -> "nyi"

let private parseSpriteEvent depthSkipper =
    depthSkipper
    >>. (tuple4 (((satisfy (isAnyOf "FMSVRCP") |>> string) <|> pstring "MX" <|> pstring "MY") .>> comma .>> spaces)
             (parseInt .>> comma .>> spaces |>> enum) (parseNum .>> comma .>> spaces) (parseNum .>> comma .>> spaces))
    >>= fun (eventType, easing, startTime, endTime) ->
        let parse2Nums = ((parseNum .>> comma) .>>. parseNum)
        match eventType with
        | "F" -> parse2Nums |>> fun (f1, f2) -> Fade(startTime, endTime, easing, f1, f2)
        | "M" -> parse2Nums .>> comma .>>. parse2Nums |>> fun (p1, p2) -> Move(startTime, endTime, easing, p1, p2)
        | "MX" -> parse2Nums |>> fun (f1, f2) -> Move_X(startTime, endTime, easing, f1, f2)
        | "MY" -> parse2Nums |>> fun (f1, f2) -> Move_Y(startTime, endTime, easing, f1, f2)
        | "S" -> parseNum |>> fun s -> Scale(startTime, endTime, easing, s)
        | "V" -> parse2Nums |>> fun p -> Vector_Scale(startTime, endTime, easing, p)
        | "R" -> parse2Nums |>> fun (f1, f2) -> Rotate(startTime, endTime, easing, f1, f2)
        | "P" ->
            (charReturn 'H' HorizontalFlip <|> charReturn 'V' VerticalFlip <|> charReturn 'A' AdditiveBlendColor)
            |>> fun p -> Parameter(startTime, endTime, p)
        | "C" ->
            (tuple3 (parseInt .>> comma) (parseInt .>> comma) (parseInt .>> comma))
            .>>. (tuple3 (parseInt .>> comma) (parseInt .>> comma) parseInt)
            |>> fun (c1, c2) -> Color(startTime, endTime, easing, c1, c2)
        //Loops cannot be parsed for now due to not using any of the same variables
        | _ -> failwith "Unknown storyboard event"

let private parseSpriteEvents depthSkipper = many (parseSpriteEvent depthSkipper .>> pchar '\n')
//todo: complete this part of the parser
let parseStoryboardEvent =
    (pstring "Animation" >>. comma
     >>. (tuple3
              (tuple4 (parseName .>> comma |>> Layer.Parse) (parseName .>> comma |>> SpriteOrigin.Parse)
                   (parseQuote .>> comma) (parseNum .>> comma))
              ((tuple4 (parseNum .>> comma) (parseInt .>> comma) (parseNum .>> comma) (parseName |>> LoopType.Parse))
               .>> pchar '\n') (parseSpriteEvents (pchar '_' <|> pchar ' ')))
     |>> fun ((layer, origin, file, x), (y, frames, frameTime, loopType), events) ->
         Animation(layer, origin, file, (x, y), frames, frameTime, loopType, events))

    <|> (pstring "Sprite" >>. comma
         >>. ((tuple5 (parseName .>> comma |>> Layer.Parse) (parseName .>> comma |>> SpriteOrigin.Parse)
                   (parseQuote .>> comma) (parseNum .>> comma) parseNum) .>> pchar '\n')
         .>>. (parseSpriteEvents (pchar '_' <|> pchar ' '))
         |>> fun ((layer, origin, file, x, y), events) -> Sprite(layer, origin, file, (x, y), events))

    <|> ((pstring "Background" <|> pstring "0") >>. comma
         >>. (tuple4 (parseNum .>> comma) (parseQuote .>> comma) (parseNum .>> comma) parseNum)
         |>> fun (time, file, x, y) -> Background(file, (x, y)))
         
    <|> ((pstring "Break" <|> pstring "2") >>. comma
        >>.  (parseNum .>> comma) .>>. parseNum
        |>> Break)

let parseEvents =
    pstring "[Events]" >>. newline >>. many (pstring "//" >>. restOfLine true)
    >>. many ((parseStoryboardEvent .>> spaces) .>> (many (pstring "//" >>. restOfLine true))) .>> spaces

(*
    Verification tools to convert metadata to usable types / prevent invalid values
*)

type GameMode =
    | osu = 0
    | Taiko = 1
    | Catch = 2
    | Mania = 3

type OverlayPosition =
    | NoChange = 0
    | Below = 1
    | Above = 2

type General =
    { AudioFilename: string
      AudioLeadIn: int
      PreviewTime: Offset
      Countdown: int
      SampleSet: SampleSet
      StackLeniency: float
      Mode: GameMode
      LetterboxInBreaks: bool
      UseSkinSprites: bool
      OverlayPosition: OverlayPosition
      SkinPreference: string
      EpilepsyWarning: bool
      CountdownOffset: int
      SpecialStyle: bool
      WidescreenStoryboard: bool
      SamplesMatchPlaybackRate: bool }

let GeneralDefault: General =
    { AudioFilename = ""
      AudioLeadIn = 0
      PreviewTime = -1.0
      Countdown = 0
      SampleSet = SampleSet.Normal
      StackLeniency = 0.7
      Mode = GameMode.osu
      LetterboxInBreaks = false
      UseSkinSprites = false
      OverlayPosition = OverlayPosition.NoChange
      SkinPreference = ""
      EpilepsyWarning = false
      CountdownOffset = 0
      SpecialStyle = false
      WidescreenStoryboard = false
      SamplesMatchPlaybackRate = false }

let private readGeneral (title, settings) =
    assert (title = "General")
    let readBool = int >> fun x -> x <> 0

    let f s (key, value) =
        match key with
        | "AudioFilename" -> { s with AudioFilename = value }
        | "AudioLeadIn" -> { s with AudioLeadIn = value |> int }
        | "PreviewTime" -> { s with PreviewTime = value |> float }
        | "Countdown" -> { s with Countdown = value |> int }
        | "SampleSet" -> { s with SampleSet = value |> SampleSet.Parse }
        | "StackLeniency" -> { s with StackLeniency = value |> float }
        | "Mode" -> { s with Mode = (value |> int |> enum) }
        | "LetterboxInBreaks" -> { s with LetterboxInBreaks = value |> readBool }
        | "UseSkinSprites" -> { s with UseSkinSprites = value |> readBool }
        | "OverlayPosition" -> { s with OverlayPosition = value |> OverlayPosition.Parse }
        | "SkinPreference" -> { s with SkinPreference = value }
        | "EpilepsyWarning" -> { s with EpilepsyWarning = value |> readBool }
        | "CountdownOffset" -> { s with CountdownOffset = value |> int }
        | "SpecialStyle" -> { s with SpecialStyle = value |> readBool }
        | "WidescreenStoryboard" -> { s with WidescreenStoryboard = value |> readBool }
        | "SamplesMatchPlaybackRate" -> { s with SamplesMatchPlaybackRate = value |> readBool }
        | _ -> s
    List.fold f GeneralDefault settings

let private writeGeneral (data : General) : Header =
    ("General", [("AudioFilename", data.AudioFilename); ("AudioLeadIn", string data.AudioLeadIn); ("PreviewTime", string data.PreviewTime);
        ("Countdown", string data.Countdown); ("SampleSet", string data.SampleSet); ("StackLeniency", string data.StackLeniency);
        ("Mode", data.Mode |> int |> string); ("LetterboxInBreaks", if data.LetterboxInBreaks then "1" else "0");
        ("UseSkinSprites", if data.UseSkinSprites then "1" else "0"); ("OverlayPosition", string data.OverlayPosition);
        ("SkinPreference", data.SkinPreference); ("EpilepsyWarning", if data.EpilepsyWarning then "1" else "0");
        ("CountdownOffset", string data.CountdownOffset); ("SpecialStyle", if data.SpecialStyle then "1" else "0");
        ("WidescreenStoryboard", if data.WidescreenStoryboard then "1" else "0"); ("SamplesMatchPlaybackRate", if data.SamplesMatchPlaybackRate then "1" else "0");])


type Editor =
    { Bookmarks: Offset list
      DistanceSpacing: float
      BeatDivisor: float
      GridSize: int
      TimelineZoom: float }

let EditorDefault: Editor =
    { Bookmarks = []
      DistanceSpacing = 1.0
      BeatDivisor = 8.0
      GridSize = 4
      TimelineZoom = 1.0 }

let private readEditor (title, settings) =
    assert (title = "Editor")
    let f s (key, value) =
        match key with
        | "Bookmarks" ->
            match run (sepBy parseNum comma) value with
            | Success(result, _, _) -> { s with Bookmarks = result }
            | Failure(errorMsg, _, _) -> failwith errorMsg
        | "DistanceSpacing" -> { s with DistanceSpacing = value |> float }
        | "BeatDivisor" -> { s with BeatDivisor = value |> float }
        | "GridSize" -> { s with GridSize = value |> int }
        | "TimelineZoom" -> { s with TimelineZoom = value |> float }
        | _ -> s
    List.fold f EditorDefault settings

let private writeEditor (data : Editor) : Header =
    ("Editor", [("Bookmarks", String.concat "," (List.map string data.Bookmarks)); ("DistanceSpacing", string data.DistanceSpacing);
        ("BeatDivisor", string data.BeatDivisor); ("GridSize", string data.GridSize); ("TimelineZoom", string data.TimelineZoom)])


type Metadata =
    { Title: string
      TitleUnicode: string
      Artist: string
      ArtistUnicode: string
      Creator: string
      Version: string
      Source: string
      Tags: string list
      BeatmapID: int
      BeatmapSetID: int }

let MetadataDefault: Metadata =
    { Title = ""
      TitleUnicode = ""
      Artist = ""
      ArtistUnicode = ""
      Creator = ""
      Version = ""
      Source = ""
      Tags = []
      BeatmapID = 0
      BeatmapSetID = -1 }

let private readMetadata (title, settings) =
    assert (title = "Metadata")
    let f s (key, value) =
        match key with
        | "Tags" ->
            match run (sepBy (many1Satisfy (Text.IsWhitespace >> not)) (many1 (pchar ' '))) value with
            | Success(result, _, _) -> { s with Tags = result }
            | Failure(errorMsg, _, _) -> failwith (errorMsg + "|" + value + "|")
        | "Title" -> { s with Title = value }
        | "TitleUnicode" -> { s with TitleUnicode = value }
        | "Artist" -> { s with Artist = value }
        | "ArtistUnicode" -> { s with ArtistUnicode = value }
        | "Creator" -> { s with Creator = value }
        | "Version" -> { s with Version = value }
        | "Source" -> { s with Source = value }
        | "BeatmapID" -> { s with BeatmapID = value |> int }
        | "BeatmapSetID" -> { s with BeatmapSetID = value |> int }
        | _ -> s
    List.fold f MetadataDefault settings

let private writeMetadata (data : Metadata) : Header =
    ("Metadata", [("Title", data.Title); ("TitleUnicode", data.TitleUnicode); ("Artist", data.Artist);
        ("ArtistUnicode", data.ArtistUnicode); ("Creator", data.Creator); ("Version", data.Version);
        ("Source", data.Source); ("Tags", String.concat " " data.Tags); ("BeatmapID", string data.BeatmapID);
        ("BeatmapSetID", string data.BeatmapSetID)])


type Difficulty =
    { HPDrainRate: float
      CircleSize: float
      OverallDifficulty: float
      ApproachRate: float
      SliderMultiplier: float
      SliderTickRate: float }

let DifficultyDefault: Difficulty =
    { HPDrainRate = 5.0
      CircleSize = 5.0
      OverallDifficulty = 5.0
      ApproachRate = 5.0
      SliderMultiplier = 1.4
      SliderTickRate = 1.0 }

let private readDifficulty (title, settings) =
    assert (title = "Difficulty")
    let f s (key, value) =
        match key with
        | "HPDrainRate" -> { s with HPDrainRate = value |> float }
        | "CircleSize" -> { s with CircleSize = value |> float }
        | "OverallDifficulty" -> { s with OverallDifficulty = value |> float }
        | "ApproachRate" -> { s with ApproachRate = value |> float }
        | "SliderMultiplier" -> { s with SliderMultiplier = value |> float }
        | "SliderTickRate" -> { s with SliderTickRate = value |> float }
        | _ -> s
    List.fold f DifficultyDefault settings

let private writeDifficulty (data : Difficulty) : Header =
    ("Difficulty", [("HPDrainRate", string data.HPDrainRate); ("CircleSize", string data.CircleSize);
        ("OverallDifficulty", string data.OverallDifficulty); ("ApproachRate", string data.ApproachRate);
        ("SliderMultiplier", string data.SliderMultiplier); ("SliderTickRate", string data.SliderTickRate)])

(*
    Final parsing of .osu file into meaningful data/saving .osu files from internal representation
*)

type Beatmap = General * Editor * Metadata * Difficulty * StoryboardObject list * HitObject list * TimingPoint list

let beatmapToString (general, editor, meta, diff, events, objects, timing) = 
    String.concat "\n\n" [
        "osu file format v14";
        general |> writeGeneral |> formatHeader;
        editor |> writeEditor |> formatHeader;
        meta |> writeMetadata |> formatHeader;
        diff |> writeDifficulty |> formatHeader;
        "[Events]\n" + String.concat "\n" (List.map (fun o -> o.ToString()) events);
        "[TimingPoints]\n" + String.concat "\n" (List.map (fun o -> o.ToString()) timing);
        "[HitObjects]\n" + String.concat "\n" (List.map (fun o -> o.ToString()) objects);
    ]

let parseBeatmap =
    tuple4 (pstring "osu file format v" >>. restOfLine true .>> spaces) (parseHeader .>> spaces)  //General
        (parseHeader .>> spaces) (parseHeader .>> spaces)  //Editor
    .>>. //Metadata
    tuple4 (parseHeader .>> spaces) (parseEvents .>> spaces) (parseTimingPoints .>> spaces)  //Difficulty
        (parseHitObjects .>> spaces)
    |>> fun ((format, general, editor, metadata), (difficulty, events, timingpoints, hitobjects)) ->
        (readGeneral general, readEditor editor, readMetadata metadata, readDifficulty difficulty, events, hitobjects,
         timingpoints)

let loadBeatmapFile path : Beatmap =
    match runParserOnFile parseBeatmap () path System.Text.Encoding.UTF8 with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let saveBeatmapFile path beatmap = 
    System.IO.File.WriteAllText(path, beatmapToString beatmap)

let getGameMode ((g, _, _, _, _, _, _): Beatmap) = g.Mode
