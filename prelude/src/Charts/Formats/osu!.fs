namespace Prelude.Charts.Formats

open FParsec
open Prelude

//https://osu.ppy.sh/help/wiki/osu!_File_Formats/Osu_(file_format)
//https://osu.ppy.sh/community/forums/topics/1869?start=12468#p12468
(*
  Not currently supported:
    Sliders (parsing or writing)
    Combo colors/starters
    Loops in storyboard (parsing or writing)
    Parsing of storyboard shorthand notation
*)

module ``osu!`` =

    (*
        Basic .osu file representation
        Headers are just decoded data and then parsed into record structures with the data tags
    *)

    type Header = string * (string * string) list

    let private format_header (title, entries) =
        let f (key, value) = key + ":" + value
        "[" + title + "]\n" + (String.concat "\n" (List.map f entries))

    type TimingEffect =
        | None = 0
        | Kiai = 1
        | OmitFirstBarline = 8

    type SampleSet =
        | None = 0
        | Default = 0
        | Normal = 1
        | Soft = 2
        | Drum = 3

    type TimingPoint =
        | BPM of Time * float32<ms / beat> * int<beat> * (SampleSet * int * int) * TimingEffect
        | SV of Time * float32 * (SampleSet * int * int) * TimingEffect
        override this.ToString() =
            match this with
            | BPM(time, msPerBeat, meter, (set, index, volume), effect) ->
                String.concat
                    ","
                    [
                        string time
                        string msPerBeat
                        string meter
                        set |> int |> string
                        string index
                        string volume
                        "1"
                        effect |> int |> string
                    ]
            | SV(time, value, (set, index, volume), effect) ->
                String.concat
                    ","
                    [
                        string time
                        string (-100.0f / value)
                        "4"
                        set |> int |> string
                        string index
                        string volume
                        "0"
                        effect |> int |> string
                    ]

    type Point = float * float

    type SliderShape =
        | Linear
        | Catmull
        | Bezier
        | PerfectCircle
        override this.ToString() =
            match this with
            | Linear -> "L"
            | Catmull -> "C"
            | Bezier -> "B"
            | PerfectCircle -> "P"

    type HitAddition = SampleSet * SampleSet * int * int * string

    let private format_hit_addition (s1, s2, index, volume, file) =
        String.concat ":" [ s1 |> int |> string; s2 |> int |> string; string index; string volume; file ]

    type HitSound =
        | Normal = 1
        | Whistle = 2
        | Finish = 4
        | Clap = 8

    type HitObject =
        | HitCircle of Point * Time * HitSound * HitAddition
        | HoldNote of Point * Time * Time * HitSound * HitAddition
        | Slider of
            Point *
            Time *
            int *
            (SliderShape * Point list) *
            float *
            HitSound *
            (HitSound list * (SampleSet * SampleSet) list * HitAddition) option
        | Spinner of Time * Time * HitSound * HitAddition
        member this.Time =
            match this with
            | HitCircle(_, time, _, _) -> time
            | HoldNote(_, time, _, _, _) -> time
            | Slider(_, time, _, _, _, _, _) -> time
            | Spinner(_, time, _, _) -> time

        override this.ToString() =
            match this with
            | HitCircle((x, y), offset, hs, addition) ->
                String.concat
                    ","
                    [
                        x |> string
                        y |> string
                        offset |> string
                        "1"
                        hs |> int |> string
                        format_hit_addition addition
                    ]
            | HoldNote((x, y), start, finish, hs, addition) ->
                String.concat
                    ","
                    [
                        string x
                        string y
                        string start
                        "128"
                        hs |> int |> string
                        (string finish) + ":" + format_hit_addition addition
                    ]
            | Slider((x, y), offset, repeats, points, length, hs, sounds) -> "nyi"
            | Spinner(start, finish, hs, addition) ->
                String.concat
                    ","
                    [
                        "320"
                        "240"
                        string start
                        "8"
                        hs |> int |> string
                        string finish
                        format_hit_addition addition
                    ]

    (*
        Storyboard data types
    *)

    //There's actually 35 of these. todo: maybe include them all?
    //https://osu.ppy.sh/wiki/en/Storyboard_Scripting/Commands

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
        | Fade of Time * Time * Easing * float * float
        | Move of Time * Time * Easing * Point * Point
        | Move_X of Time * Time * Easing * float * float
        | Move_Y of Time * Time * Easing * float * float
        | Scale of Time * Time * Easing * float * float
        | VectorScale of Time * Time * Easing * Point * Point
        | Rotate of Time * Time * Easing * float * float
        | Color of Time * Time * Easing * (int * int * int) * (int * int * int)
        | Loop of Time * int * StoryboardEvent list
        | Trigger_Loop of Time * Time * TriggerType * StoryboardEvent list
        | Parameter of Time * Time * SpriteParameter
        member this.Format padding =
            let strint: Time -> string = int >> string

            padding
            + (String.concat
                ","
                (match this with
                 | Fade(time1, time2, easing, a, b) ->
                     [ "F"; easing |> int |> string; strint time1; strint time2; string a; string b ]
                 | Move(time1, time2, easing, (x1, y1), (x2, y2)) ->
                     [
                         "M"
                         easing |> int |> string
                         strint time1
                         strint time2
                         string x1
                         string y1
                         string x2
                         string y2
                     ]
                 | Move_X(time1, time2, easing, a, b) ->
                     [
                         "MX"
                         easing |> int |> string
                         strint time1
                         strint time2
                         string a
                         string b
                     ]
                 | Move_Y(time1, time2, easing, a, b) ->
                     [
                         "MY"
                         easing |> int |> string
                         strint time1
                         strint time2
                         string a
                         string b
                     ]
                 | Scale(time1, time2, easing, a, b) ->
                     [ "S"; easing |> int |> string; strint time1; strint time2; string a; string b ]
                 | VectorScale(time1, time2, easing, (x1, y1), (x2, y2)) ->
                     [
                         "V"
                         easing |> int |> string
                         strint time1
                         strint time2
                         string x1
                         string y1
                         string x2
                         string y2
                     ]
                 | Rotate(time1, time2, easing, a, b) ->
                     [ "R"; easing |> int |> string; strint time1; strint time2; string a; string b ]
                 | Color(time1, time2, easing, (r1, g1, b1), (r2, g2, b2)) ->
                     [
                         "C"
                         easing |> int |> string
                         strint time1
                         strint time2
                         string r1
                         string g1
                         string b1
                         string r2
                         string g2
                         string b2
                     ]
                 | _ -> [ "nyi" ]))

    type StoryboardObject =
        | Sprite of Layer * SpriteOrigin * string * Point * StoryboardEvent list
        | Animation of Layer * SpriteOrigin * string * Point * int * Time * LoopType * StoryboardEvent list
        | Sample of Time * Layer * string * int
        | Background of string * Point
        | Video of Time * string * Point
        | Break of Time * Time
        | BackgroundColorChange of Time * int * int * int //i have no idea what this is or does
        override this.ToString() =
            match this with
            | Background(filename, (x, y)) -> String.concat "," [ "0"; "0"; "\"" + filename + "\""; string x; string y ]
            | Video(time, filename, (x, y)) ->
                String.concat "," [ "1"; string time; "\"" + filename + "\""; string x; string y ]
            | Break(start, finish) -> String.concat "," [ "2"; string start; string finish ]
            | Sprite(layer, origin, filename, (x, y), events) ->
                String.concat
                    "\n"
                    ((String.concat
                        ","
                        [
                            "Sprite"
                            layer.ToString("G")
                            origin.ToString("G")
                            "\"" + filename + "\""
                            string x
                            string y
                        ])
                     :: (List.map (fun (x: StoryboardEvent) -> x.Format " ") events))
            | _ -> "nyi"

    (*
        Parsing of .osu and .osb formats
    *)

    let private is_letter_or_digit c = isLetter c || isDigit c
    let private comma = pchar ','
    let private colon = pchar ':'
    let private pipe = pchar '|'
    let private parse_num = pfloat
    let private parse_int = pint64 |>> int
    let private parse_name = many1Satisfy is_letter_or_digit

    let private parse_quote =
        between (pchar '"') (pchar '"') (manySatisfy (fun c -> c <> '"'))
        <|> (many1Satisfy (Text.IsWhitespace >> not))

    let private comment = (anyOf "-/#=" >>% "") >>. restOfLine true .>> spaces

    let private parse_key_value =
        parse_name .>> spaces .>> colon .>> manyChars (anyOf " \t")
        .>>. (restOfLine true)
        .>> spaces

    let private parse_header_title name =
        pstring ("[" + name + "]") .>> (restOfLine true) .>> spaces >>% name

    let parse_header name : Parser<Header, unit> =
        many comment >>. parse_header_title name
        .>>. (many comment >>. many (parse_key_value .>> (many comment)))
        .>> spaces
        |>> Header

    let private parse_timing_point: Parser<TimingPoint, unit> =
        (tuple4 (parse_num .>> comma) (parse_num .>> comma) (parse_int .>> comma) (parse_int .>> comma))
        .>>. (tuple4 (parse_int .>> comma) (parse_int .>> comma) (parse_int .>> comma) parse_int)
        |>> fun ((offset, value, meter, sampleSet), (sampleIndex, volume, isBpm, effects)) ->
            if isBpm > 0 then
                BPM(
                    Time.ofFloat offset,
                    Time.ofFloat value / 1.0f<beat>,
                    meter * 1<beat>,
                    (enum sampleSet, sampleIndex, volume),
                    enum effects
                )
            else
                SV(Time.ofFloat offset, -100.0f / float32 value, (enum sampleSet, sampleIndex, volume), enum effects)

    let private parse_timing_points =
        pstring "[TimingPoints]" >>. newline >>. many (parse_timing_point .>> newline)

    let private parse_point = parse_num .>> comma .>>. parse_num |>> Point
    let private parse_point_colon = parse_num .>> colon .>>. parse_num |>> Point

    let private parse_addition: Parser<HitAddition, unit> =
        attempt (
            tuple5
                (parse_int .>> colon)
                (parse_int .>> colon)
                (parse_int .>> colon)
                (parse_int .>> colon)
                (restOfLine true)
            |>> fun (normal, addition, index, volume, file) -> (enum normal, enum addition, index, volume, file)
        )
        // for parsing and discarding legacy hitsounds (there is no documentation)
        <|> ((parse_int >>. colon >>. parse_int >>. colon >>. parse_int >>. restOfLine true)
             >>. preturn (enum 0, enum 0, 0, 0, ""))

    let private parse_slider_type =
        anyOf "LBCP"
        |>> fun c ->
            match c with
            | 'L' -> Linear
            | 'B' -> Bezier
            | 'C' -> Catmull
            | 'P' -> PerfectCircle
            | _ -> failwith "impossible"

    let private parse_slider_sounds =
        (sepBy1 (parse_int |>> enum) pipe) .>> comma
        .>>. (sepBy1 ((parse_int |>> enum) .>> colon .>>. (parse_int |>> enum)) pipe)

    let private parse_slider_points =
        parse_slider_type .>> pipe .>>. sepBy1 parse_point_colon pipe

    let private parse_hit_object: Parser<HitObject, unit> =
        tuple4 (parse_point .>> comma) (parse_num .>> comma) (parse_int .>> comma) ((parse_int .>> comma) |>> enum)
        >>= (fun (pos, offset, objType, hitsound) ->
            let offset = Time.ofFloat offset

            match objType &&& 139 with
            | 1 -> parse_addition |>> fun addition -> HitCircle(pos, offset, hitsound, addition)
            | 2 ->
                tuple4
                    (parse_slider_points .>> comma)
                    (parse_int .>> comma)
                    parse_num
                    (opt (comma >>. (parse_slider_sounds .>> comma) .>>. parse_addition)
                     |>> Option.map (fun ((a, b), c) -> (a, b, c)))
                |>> fun (points, slides, length, sounds) ->
                    Slider(pos, offset, slides, points, length, hitsound, sounds)
            | 8 ->
                pipe2
                    (parse_num .>> comma)
                    parse_addition
                    (fun endTime addition -> Spinner(offset, Time.ofFloat endTime, hitsound, addition))
            | 128 ->
                pipe2
                    (parse_num .>> colon)
                    parse_addition
                    (fun endTime addition -> HoldNote(pos, offset, Time.ofFloat endTime, hitsound, addition))
            | _ -> failwith "Unknown hitobject type"
        )

    let private parse_hit_objects =
        pstring "[HitObjects]" >>. newline >>. many parse_hit_object

    (*
        Storyboard parsing code
    *)

    let rec private parse_sprite_event depth_skip =
        depth_skip
        >>. ((pchar 'L' >>. fail "Storyboard loops are not supported!")
             <|>
             //((pchar 'L' >>. comma >>. tuple3 (parseNum .>> comma) (parseInt .>> comma .>> spaces) (many (parseSpriteEvent ((pchar ' ' <|> pchar '_') >>. depthSkipper .>> skipNewline))))
             //|>> (fun (time, repeats, events) -> Loop(time, repeats, events)))
             //<|>
             (tuple4
                 ((pstring "MX" <|> pstring "MY" <|> (satisfy (isAnyOf "FMSVRCP") |>> string))
                  .>> comma
                  .>> spaces)
                 (parse_int .>> comma .>> spaces |>> enum)
                 (opt parse_num .>> comma .>> spaces)
                 (opt parse_num .>> comma .>> spaces)
              >>= (fun (eventType, easing, startTime, endTime) ->
                  let parsePoint = ((parse_num .>> comma) .>>. parse_num)

                  let parse_nums_with_shorthand =
                      (parse_num .>>. opt (comma >>. parse_num))
                      |>> fun (a, b) -> (a, Option.defaultValue a b)

                  let startTime, endTime =
                      match startTime, endTime with
                      | None, None -> failwith "Both start time and end time were left blank"
                      | Some x, None -> x, x
                      | None, Some x -> x, x
                      | Some x, Some y -> x, y

                  let startTime, endTime = Time.ofFloat startTime, Time.ofFloat endTime

                  match eventType with
                  | "F" ->
                      parse_nums_with_shorthand
                      |>> fun (f1, f2) -> Fade(startTime, endTime, easing, f1, f2)
                  | "M" ->
                      parsePoint .>>. opt (comma >>. parsePoint)
                      |>> fun (p1, p2) -> Move(startTime, endTime, easing, p1, Option.defaultValue p1 p2)
                  | "MX" ->
                      parse_nums_with_shorthand
                      |>> fun (f1, f2) -> Move_X(startTime, endTime, easing, f1, f2)
                  | "MY" ->
                      parse_nums_with_shorthand
                      |>> fun (f1, f2) -> Move_Y(startTime, endTime, easing, f1, f2)
                  | "S" ->
                      parse_nums_with_shorthand
                      |>> fun (s1, s2) -> Scale(startTime, endTime, easing, s1, s2)
                  | "V" ->
                      parsePoint .>>. opt (comma >>. parsePoint)
                      |>> fun (p1, p2) -> VectorScale(startTime, endTime, easing, p1, Option.defaultValue p1 p2)
                  | "R" ->
                      parse_nums_with_shorthand
                      |>> fun (f1, f2) -> Rotate(startTime, endTime, easing, f1, f2)
                  | "P" ->
                      (charReturn 'H' HorizontalFlip
                       <|> charReturn 'V' VerticalFlip
                       <|> charReturn 'A' AdditiveBlendColor)
                      |>> fun p -> Parameter(startTime, endTime, p)
                  | "C" ->
                      (tuple3 (parse_int .>> comma) (parse_int .>> comma) (parse_int))
                      .>>. opt (tuple3 (comma >>. parse_int .>> comma) (parse_int .>> comma) parse_int)
                      |>> fun (c1, c2) -> Color(startTime, endTime, easing, c1, Option.defaultValue c1 c2)
                  | _ -> failwith "Unknown storyboard event"
              )))

    let private parse_sprite_events depthSkipper =
        many (parse_sprite_event depthSkipper .>> skipNewline)

    let private parse_storyboard_event =
        (pstring "Animation"
         >>. comma
         >>. (tuple3
             (tuple4
                 (parse_name .>> comma |>> Layer.Parse)
                 (parse_name .>> comma |>> SpriteOrigin.Parse)
                 (parse_quote .>> comma)
                 (parse_num .>> comma))
             ((tuple4 (parse_num .>> comma) (parse_int .>> comma) (parse_num .>> comma) (parse_name |>> LoopType.Parse))
              .>> pchar '\n')
             (parse_sprite_events (pchar '_' <|> pchar ' ')))
         |>> fun ((layer, origin, file, x), (y, frames, frameTime, loopType), events) ->
             Animation(layer, origin, file, (x, y), frames, Time.ofFloat frameTime, loopType, events))

        <|> ((pstring "Sprite" <|> pstring "4")
             >>. comma
             >>. ((tuple5
                      (parse_name .>> comma |>> Layer.Parse)
                      (parse_name .>> comma |>> SpriteOrigin.Parse)
                      (parse_quote .>> comma)
                      (parse_num .>> comma)
                      parse_num)
                  .>> pchar '\n')
             .>>. (parse_sprite_events (pchar '_' <|> pchar ' '))
             |>> fun ((layer, origin, file, x, y), events) -> Sprite(layer, origin, file, (x, y), events))

        <|> ((pstring "Background" <|> pstring "0")
             >>. comma
             >>. (tuple3
                 (parse_num .>> comma)
                 parse_quote
                 (opt (tuple2 (comma >>. parse_num) (comma >>. parse_num))
                  |>> Option.defaultValue (0.0, 0.0)))
             |>> fun (time, file, (x, y)) -> Background(file, (x, y)))

        <|> ((pstring "Sample")
             >>. comma
             >>. (tuple4 (parse_num .>> comma) (parse_name .>> comma |>> Layer.Parse) (parse_quote .>> comma) parse_int)
             |>> fun (time, layer, file, volume) -> Sample(Time.ofFloat time, layer, file, volume))

        <|> ((pstring "Video" <|> pstring "1")
             >>. comma
             >>. (tuple3
                 (parse_num .>> comma)
                 parse_quote
                 (opt (tuple2 (comma >>. parse_num) (comma >>. parse_num))
                  |>> Option.defaultValue (0.0, 0.0)))
             |>> fun (time, file, (x, y)) -> Video(Time.ofFloat time, file, (x, y)))

        <|> ((pstring "Break" <|> pstring "2") >>. comma >>. (parse_num .>> comma)
             .>>. parse_num
             |>> fun (time1, time2) -> Break(Time.ofFloat time1, Time.ofFloat time2))

        //I have no idea what this does and cannot for the life of me find any documentation on it
        <|> (pstring "3"
             >>. comma
             >>. (tuple4 (parse_num .>> comma) (parse_int .>> comma) (parse_int .>> comma) (parse_int))
             |>> fun (time, r, g, b) -> BackgroundColorChange(Time.ofFloat time, r, g, b))

    let private parse_events =
        pstring "[Events]"
        >>. newline
        >>. many comment
        >>. many ((parse_storyboard_event .>> spaces) .>> (many comment))
        .>> spaces

    (*
        Metadata structures to turn parsed headers into the meaningful metadata
        Can also check here for invalid values/ignore unknown values
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
        {
            AudioFilename: string
            AudioLeadIn: int
            PreviewTime: int
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
            SamplesMatchPlaybackRate: bool
        }
        static member Default =
            {
                AudioFilename = ""
                AudioLeadIn = 0
                PreviewTime = -1
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
                SamplesMatchPlaybackRate = false
            }

    let private read_general (title, settings) =
        assert (title = "General")
        let read_bool = int >> fun x -> x <> 0

        let f s (key, value) =
            match key with
            | "AudioFilename" -> { s with AudioFilename = value }
            | "AudioLeadIn" -> { s with AudioLeadIn = value |> int }
            | "PreviewTime" ->
                { s with
                    PreviewTime = value |> int
                }
            | "Countdown" -> { s with Countdown = value |> int }
            | "SampleSet" ->
                { s with
                    SampleSet = value |> SampleSet.Parse
                }
            | "StackLeniency" ->
                { s with
                    StackLeniency = value |> float
                }
            | "Mode" -> { s with Mode = (value |> int |> enum) }
            | "LetterboxInBreaks" ->
                { s with
                    LetterboxInBreaks = value |> read_bool
                }
            | "UseSkinSprites" ->
                { s with
                    UseSkinSprites = value |> read_bool
                }
            | "OverlayPosition" ->
                { s with
                    OverlayPosition = value |> OverlayPosition.Parse
                }
            | "SkinPreference" -> { s with SkinPreference = value }
            | "EpilepsyWarning" ->
                { s with
                    EpilepsyWarning = value |> read_bool
                }
            | "CountdownOffset" ->
                { s with
                    CountdownOffset = value |> int
                }
            | "SpecialStyle" ->
                { s with
                    SpecialStyle = value |> read_bool
                }
            | "WidescreenStoryboard" ->
                { s with
                    WidescreenStoryboard = value |> read_bool
                }
            | "SamplesMatchPlaybackRate" ->
                { s with
                    SamplesMatchPlaybackRate = value |> read_bool
                }
            | _ -> s

        List.fold f General.Default settings

    let private write_general (data: General) : Header =
        ("General",
         [
             ("AudioFilename", data.AudioFilename)
             ("AudioLeadIn", string data.AudioLeadIn)
             ("PreviewTime", string data.PreviewTime)
             ("Countdown", string data.Countdown)
             ("SampleSet", string data.SampleSet)
             ("StackLeniency", string data.StackLeniency)
             ("Mode", data.Mode |> int |> string)
             ("LetterboxInBreaks", (if data.LetterboxInBreaks then "1" else "0"))
             ("UseSkinSprites", (if data.UseSkinSprites then "1" else "0"))
             ("OverlayPosition", string data.OverlayPosition)
             ("SkinPreference", data.SkinPreference)
             ("EpilepsyWarning", (if data.EpilepsyWarning then "1" else "0"))
             ("CountdownOffset", string data.CountdownOffset)
             ("SpecialStyle", (if data.SpecialStyle then "1" else "0"))
             ("WidescreenStoryboard", (if data.WidescreenStoryboard then "1" else "0"))
             ("SamplesMatchPlaybackRate", (if data.SamplesMatchPlaybackRate then "1" else "0"))
         ])


    type Editor =
        {
            Bookmarks: Time list
            DistanceSpacing: float
            BeatDivisor: float
            GridSize: int
            TimelineZoom: float
        }
        static member Default =
            {
                Bookmarks = []
                DistanceSpacing = 1.0
                BeatDivisor = 8.0
                GridSize = 4
                TimelineZoom = 1.0
            }

    let private read_editor (title, settings) =
        assert (title = "Editor")

        let f s (key, value) =
            match key with
            | "Bookmarks" ->
                match run (sepBy parse_num comma) value with
                | Success(result, _, _) ->
                    { s with
                        Bookmarks = result |> List.map Time.ofFloat
                    }
                | Failure(errorMsg, _, _) -> failwith errorMsg
            | "DistanceSpacing" ->
                { s with
                    DistanceSpacing = value |> float
                }
            | "BeatDivisor" -> { s with BeatDivisor = value |> float }
            | "GridSize" -> { s with GridSize = value |> int }
            | "TimelineZoom" -> { s with TimelineZoom = value |> float }
            | _ -> s

        List.fold f Editor.Default settings

    let private write_editor (data: Editor) : Header =
        ("Editor",
         [
             ("Bookmarks", String.concat "," (List.map string data.Bookmarks))
             ("DistanceSpacing", string data.DistanceSpacing)
             ("BeatDivisor", string data.BeatDivisor)
             ("GridSize", string data.GridSize)
             ("TimelineZoom", string data.TimelineZoom)
         ])


    type Metadata =
        {
            Title: string
            TitleUnicode: string
            Artist: string
            ArtistUnicode: string
            Creator: string
            Version: string
            Source: string
            Tags: string list
            BeatmapID: int
            BeatmapSetID: int
        }
        static member Default =
            {
                Title = ""
                TitleUnicode = ""
                Artist = ""
                ArtistUnicode = ""
                Creator = ""
                Version = ""
                Source = ""
                Tags = []
                BeatmapID = 0
                BeatmapSetID = -1
            }

    let private read_metadata (title, settings) =
        assert (title = "Metadata")

        let f s (key, value: string) =
            match key with
            | "Tags" ->
                { s with
                    Tags =
                        value.Split(([||]: char array), System.StringSplitOptions.RemoveEmptyEntries)
                        |> List.ofArray
                }
            | "Title" -> { s with Title = value }
            | "TitleUnicode" -> { s with TitleUnicode = value }
            | "Artist" -> { s with Artist = value }
            | "ArtistUnicode" -> { s with ArtistUnicode = value }
            | "Creator" -> { s with Creator = value }
            | "Version" -> { s with Version = value }
            | "Source" -> { s with Source = value }
            | "BeatmapID" -> { s with BeatmapID = int value }
            | "BeatmapSetID" -> { s with BeatmapSetID = int value }
            | _ -> s

        List.fold f Metadata.Default settings

    let private write_metadata (data: Metadata) : Header =
        ("Metadata",
         [
             ("Title", data.Title)
             ("TitleUnicode", data.TitleUnicode)
             ("Artist", data.Artist)
             ("ArtistUnicode", data.ArtistUnicode)
             ("Creator", data.Creator)
             ("Version", data.Version)
             ("Source", data.Source)
             ("Tags", String.concat " " data.Tags)
             ("BeatmapID", string data.BeatmapID)
             ("BeatmapSetID", string data.BeatmapSetID)
         ])


    type Difficulty =
        {
            HPDrainRate: float
            CircleSize: float
            OverallDifficulty: float
            ApproachRate: float
            SliderMultiplier: float
            SliderTickRate: float
        }
        static member Default =
            {
                HPDrainRate = 5.0
                CircleSize = 5.0
                OverallDifficulty = 5.0
                ApproachRate = 5.0
                SliderMultiplier = 1.4
                SliderTickRate = 1.0
            }

    let private read_difficulty (title, settings) =
        assert (title = "Difficulty")

        let f s (key, value) =
            match key with
            | "HPDrainRate" -> { s with HPDrainRate = float value }
            | "CircleSize" -> { s with CircleSize = float value }
            | "OverallDifficulty" ->
                { s with
                    OverallDifficulty = float value
                }
            | "ApproachRate" -> { s with ApproachRate = float value }
            | "SliderMultiplier" ->
                { s with
                    SliderMultiplier = float value
                }
            | "SliderTickRate" -> { s with SliderTickRate = float value }
            | _ -> s

        List.fold f Difficulty.Default settings

    let private write_difficulty (data: Difficulty) : Header =
        ("Difficulty",
         [
             ("HPDrainRate", string data.HPDrainRate)
             ("CircleSize", string data.CircleSize)
             ("OverallDifficulty", string data.OverallDifficulty)
             ("ApproachRate", string data.ApproachRate)
             ("SliderMultiplier", string data.SliderMultiplier)
             ("SliderTickRate", string data.SliderTickRate)
         ])

    (*
        Final parsing of .osu file into meaningful data/saving .osu files from internal representation
    *)

    type Beatmap =
        {
            General: General
            Editor: Editor
            Metadata: Metadata
            Difficulty: Difficulty
            Events: StoryboardObject list
            Objects: HitObject list
            Timing: TimingPoint list
        }


    //todo: in future can include variables list too
    type Storyboard = StoryboardObject list

    let get_osu_filename beatmap =
        let clean =
            String.filter (fun c -> System.Char.IsWhiteSpace c || System.Char.IsLetterOrDigit c)

        sprintf
            "%s - %s (%s) [%s].osu"
            (clean beatmap.Metadata.ArtistUnicode)
            (clean beatmap.Metadata.TitleUnicode)
            (clean beatmap.Metadata.Creator)
            (clean beatmap.Metadata.Version)

    let storyboard_to_string (events: StoryboardObject list) =
        "[Events]\n" + String.concat "\n" (List.map (fun o -> o.ToString()) events)

    let beatmap_to_string beatmap =
        String.concat
            "\n\n"
            [
                "osu file format v14"
                beatmap.General |> write_general |> format_header
                beatmap.Editor |> write_editor |> format_header
                beatmap.Metadata |> write_metadata |> format_header
                beatmap.Difficulty |> write_difficulty |> format_header
                beatmap.Events |> storyboard_to_string
                "[TimingPoints]\n"
                + String.concat "\n" (List.map (fun o -> o.ToString()) beatmap.Timing)
                "[HitObjects]\n"
                + String.concat "\n" (List.map (fun o -> o.ToString()) beatmap.Objects)
            ]

    let private parse_beatmap =
        tuple4
            (spaces >>. pstring "osu file format v" >>. restOfLine true .>> spaces)
            (parse_header "General" .>> spaces) //General
            (parse_header "Editor" .>> spaces) //Editor
            (parse_header "Metadata" .>> spaces) //Metadata
        .>>. tuple5
            (parse_header "Difficulty" .>> spaces) (*Difficulty*)
            (parse_events .>> spaces)
            (parse_timing_points .>> spaces)
            (optional (parse_header "Colours" .>> spaces))
            (parse_hit_objects .>> spaces)
        |>> fun ((format, general, editor, metadata), (difficulty, events, timingpoints, _, hitobjects)) ->
            {
                General = read_general general
                Editor = read_editor editor
                Metadata = read_metadata metadata
                Difficulty = read_difficulty difficulty
                Events = events
                Objects = hitobjects
                Timing = timingpoints
            }

    open System.Text
    open System.Text.RegularExpressions
    open System.IO

    let beatmap_from_file path : Result<Beatmap, string> =
        match runParserOnFile parse_beatmap () path Encoding.UTF8 with
        | Success(result, _, _) -> Result.Ok result
        | Failure(error_message, _, _) -> Result.Error error_message

    let beatmap_to_file path beatmap =
        File.WriteAllText(path, beatmap_to_string beatmap)

    let storyboard_from_file path : Result<Storyboard, string> =
        match runParserOnFile parse_events () path Encoding.UTF8 with
        | Success(result, _, _) -> Result.Ok result
        | Failure(error_msg, _, _) -> Result.Error error_msg

    let storyboard_to_file path events =
        File.WriteAllText(path, storyboard_to_string events + "\n\n")

    let private RATE_REGEX =
        Regex(
            """((^|\s)([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)($|\s))|(x([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?))|(([02][,.][0-9][0-9]?|1[,.]0[1-9]|1[,.][1-9][0-9]?)x)"""
        )
    // Interlude supports rates, so rate mods are detected to be skipped when batch converting
    let detect_rate_mod (difficulty_name: string) : float32 option =
        let m = RATE_REGEX.Match difficulty_name

        if m.Success then
            let r = m.Value.Trim([| ' '; 'x' |]).Replace(',', '.')

            match System.Single.TryParse r with
            | true, r -> Some r
            | false, _ -> None
        else
            None
