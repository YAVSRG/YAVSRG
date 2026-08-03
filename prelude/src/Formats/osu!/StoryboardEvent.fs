namespace Prelude.Formats.Osu

type Easing =
    | Linear = 0
    | EasingOut = 1 // Decelerate
    | EasingIn = 2 // Accelerate
    | QuadIn = 3
    | QuadOut = 4
    | QuadInOut = 5
    | CubicIn = 6
    | CubicOut = 7
    | CubicInOut = 8
    | QuartIn = 9
    | QuartOut = 10
    | QuartInOut = 11
    | QuintIn = 12
    | QuintOut = 13
    | QuintInOut = 14
    | SineIn = 15
    | SineOut = 16
    | SineInOut = 17
    | ExpoIn = 18
    | ExpoOut = 19
    | ExpoInOut = 20
    | CircIn = 21
    | CircOut = 22
    | CircInOut = 23
    | ElasticIn = 24
    | ElasticOut = 25
    | ElasticHalfOut = 26
    | ElasticQuarterOut = 27
    | ElasticInOut = 28
    | BackIn = 29
    | BackOut = 30
    | BackInOut = 31
    | BounceIn = 32
    | BounceOut = 33
    | BounceInOut = 34

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

type StoryboardCommandGuts =
    | Fade of start_opacity: float * end_opacity: float
    | Move of start_pos: (int * int) * end_pos: (int * int)
    | Move_X of start_x: int * end_x: int
    | Move_Y of start_y: int * end_y: int
    | Scale of start_scale: float * end_scale: float
    | VectorScale of start_scale: (float * float) * end_scale: (float * float)
    | Rotate of start_radians: float * end_radians: float
    | Color of start_color: (int * int * int) * end_color: (int * int * int)

    member this.Shorthand =
        match this with
        | Fade _ -> "F"
        | Move _ -> "M"
        | Move_X _ -> "MX"
        | Move_Y _ -> "MY"
        | Scale _ -> "S"
        | VectorScale _ -> "V"
        | Rotate _ -> "R"
        | Color _ -> "C"

    override this.ToString() : string =
        match this with
        | Fade(a, b) -> sprintf "%O,%O" a b
        | Move((x1, y1), (x2, y2)) -> sprintf "%i,%i,%i,%i" x1 y1 x2 y2
        | Move_X(a, b) -> sprintf "%i,%i" a b
        | Move_Y(a, b) -> sprintf "%i,%i" a b
        | Scale(a, b) -> sprintf "%O,%O" a b
        | VectorScale((x1, y1), (x2, y2)) -> sprintf "%O,%O,%O,%O" x1 y1 x2 y2
        | Rotate(a, b) -> sprintf "%O,%O" a b
        | Color((r1, g1, b1), (r2, g2, b2)) -> sprintf "%i,%i,%i,%i,%i,%i" r1 g1 b1 r2 g2 b2

// todo: normal/trigger/loop DU
type StoryboardCommand =
    {
        Easing: Easing
        StartTime: int
        EndTime: int
        Command: StoryboardCommandGuts
    }

    member this.Format: string seq =
        seq {
            yield
                sprintf
                    "%s,%i,%i,%i,%O"
                    this.Command.Shorthand
                    (int this.Easing)
                    this.StartTime
                    this.EndTime
                    this.Command
        }

type Sprite =
    {
        Layer: Layer
        Origin: SpriteOrigin
        File: string
        X: int
        Y: int
        Commands: StoryboardCommand list
    }

    override this.ToString() : string =
        seq {
            yield sprintf "Sprite,%O,%O,%A,%i,%i" this.Layer this.Origin this.File this.X this.Y

            for command in this.Commands do
                for f in command.Format do
                    yield " " + f
        }
        |> String.concat "\n"

type Animation =
    {
        Layer: Layer
        Origin: SpriteOrigin
        File: string
        X: int
        Y: int
        FrameCount: int
        MsPerFrame: int
        LoopType: LoopType
        Commands: StoryboardCommand list
    }

    override this.ToString() : string =
        seq {
            yield
                sprintf
                    "Animation,%O,%O,%A,%i,%i,%i,%i,%i"
                    this.Layer
                    this.Origin
                    this.File
                    this.X
                    this.Y
                    this.FrameCount
                    this.MsPerFrame
                    (int this.LoopType)

            for command in this.Commands do
                for f in command.Format do
                    yield " " + f
        }
        |> String.concat "\n"

type StoryboardEvent =
    | Sprite of Sprite
    | Animation of Animation
    | Sample of int * Layer * string * int
    | Background of string * int * int
    | Video of int * string * int * int
    | Break of int * int

    override this.ToString() : string =
        match this with
        | Sprite sprite -> sprite.ToString()
        | Animation animation -> animation.ToString()
        | Sample(time, layer, filename, volume) -> sprintf "Sample,%i,%i,%A,%i" time (int layer) filename volume
        | Background(filename, x, y) -> sprintf "0,0,%A,%i,%i" filename x y
        | Video(time, filename, x, y) -> sprintf "1,%i,%A,%i,%i" time filename x y
        | Break(start, finish) -> sprintf "2,%i,%i" start finish

    static member TryParse(line: string) : StoryboardEvent option =
        let values = line.Split(',')

        let inline unsupported () = None

        let inline parse_background () =
            ignore(values.ValueAt(1).ParseInt().ExpectValid())

            Background(
                values.ValueAt(2).DefaultValue("").Trim().Trim('"').Trim(),
                values.ValueAt(3).ReplaceInvalidWith("0").ParseInt().ExpectValid(),
                values.ValueAt(4).ReplaceInvalidWith("0").ParseInt().ExpectValid()
            )

        let inline parse_video () =
            Video(
                values.ValueAt(1).ParseInt().ExpectValid(),
                values.ValueAt(2).DefaultValue("").Trim().Trim('"').Trim(),
                values.ValueAt(3).ReplaceInvalidWith("0").ParseInt().ExpectValid(),
                values.ValueAt(4).ReplaceInvalidWith("0").ParseInt().ExpectValid()
            )

        let inline parse_break () =
            Break(values.ValueAt(1).ParseInt().DefaultValue(0), values.ValueAt(2).ParseInt().DefaultValue(0))

        let inline parse_sample () =
            Sample(
                values.ValueAt(1).ParseInt().DefaultValue(0),
                values.ValueAt(2).ParseInt().DefaultValue(0) |> enum,
                values.ValueAt(3).DefaultValue("").Trim('"').Trim(),
                values.ValueAt(4).ParseInt().DefaultValue(0)
            )

        if line.StartsWith(' ') || line.StartsWith('_') then
            unsupported()
        else

        match values.[0].Trim() with
        | "0"
        | "Background" -> Some(parse_background())
        | "1"
        | "Video" -> Some(parse_video())
        | "2"
        | "Break" -> Some(parse_break())
        | "Sample" -> Some(parse_sample())
        | "3"
        | "Sprite"
        | "Animation" -> unsupported()
        | _ -> raise(ParseException([| line, "Unrecognised event type" |]))
