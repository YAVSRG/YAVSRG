namespace Prelude.Formats.Osu

open System
open System.Globalization

type HitSound =
    | Default = 0
    | Normal = 1
    | Whistle = 2
    | Finish = 4
    | Clap = 8

type HitCircle =
    {
        X: int
        Y: int
        Time: int
        StartsNewCombo: bool
        ColorHax: int
        HitSound: HitSound
        HitSample: HitSample
    }

    override this.ToString() : string =
        sprintf
            "%i,%i,%i,%i,%i,%O"
            this.X
            this.Y
            this.Time
            (1 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.HitSample

    static member inline Create(keys: int, key: int, time: ^T) : HitCircle =
        {
            X = (float key + 0.5) * 512.0 / float keys |> int
            Y = 240
            Time = time |> float32 |> round |> int
            StartsNewCombo = false
            ColorHax = 0
            HitSound = HitSound.Default
            HitSample = HitSample.Default
        }

type SliderShape =
    | Linear
    | Catmull
    | Bezier
    | PerfectCircle

    override this.ToString() : string =
        match this with
        | Linear -> "L"
        | Catmull -> "C"
        | Bezier -> "B"
        | PerfectCircle -> "P"

    static member FromString(shape: string) : SliderShape =
        if shape.Length <> 1 then
            raise(ParseException([| shape |]))
        else
            match shape.[0] with
            | 'L' -> Linear
            | 'B' -> Bezier
            | 'P' -> PerfectCircle
            | _ -> Catmull

type Slider =
    {
        X: int
        Y: int
        Time: int
        StartsNewCombo: bool
        ColorHax: int
        HitSound: HitSound
        CurveType: SliderShape
        CurvePoints: (int * int) list
        Slides: int
        Length: float
        EdgeSounds: HitSound list
        EdgeSets: (SampleSet * SampleSet) list
        HitSample: HitSample
    }

    member this.Repeats = this.Slides - 1

    override this.ToString() : string =
        let inline format_curve_point (x, y) : string = sprintf "%i:%i" x y

        let inline format_curve_points (ps: _ list) : string =
            if ps = [] then "" else "|" + (Seq.map format_curve_point ps |> String.concat "|")

        sprintf
            "%i,%i,%i,%i,%i,%O%s,%i,%s,%s,%s,%O"
            this.X
            this.Y
            this.Time
            (2 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.CurveType
            (format_curve_points(this.CurvePoints))
            this.Slides
            (this.Length.ToString(CultureInfo.InvariantCulture))
            (this.EdgeSounds |> Seq.map(int >> sprintf "%i") |> String.concat "|")
            (this.EdgeSets
             |> Seq.map(fun (normal, addition) -> sprintf "%i:%i" (int normal) (int addition))
             |> String.concat "|")
            this.HitSample

type Spinner =
    {
        X: int
        Y: int
        Time: int
        StartsNewCombo: bool
        ColorHax: int
        HitSound: HitSound
        EndTime: int
        HitSample: HitSample
    }

    override this.ToString() : string =
        sprintf
            "%i,%i,%i,%i,%i,%i,%O"
            this.X
            this.Y
            this.Time
            (8 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.EndTime
            this.HitSample

type Hold =
    {
        X: int
        Y: int
        Time: int
        StartsNewCombo: bool
        ColorHax: int
        HitSound: HitSound
        EndTime: int
        HitSample: HitSample
    }

    override this.ToString() : string =
        sprintf
            "%i,%i,%i,%i,%i,%i:%O"
            this.X
            this.Y
            this.Time
            (128 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.EndTime
            this.HitSample

    static member inline Create(keys: int, key: int, start_time: ^T, end_time: ^T) : Hold =
        {
            X = (float key + 0.5) * 512.0 / float keys |> int
            Y = 240
            Time = start_time |> float32 |> round |> int
            StartsNewCombo = false
            ColorHax = 0
            HitSound = HitSound.Default
            EndTime = end_time |> float32 |> round |> int
            HitSample = HitSample.Default
        }

type HitObject =
    | HitCircle of HitCircle
    | Hold of Hold
    | Slider of Slider
    | Spinner of Spinner

    member this.Time =
        match this with
        | HitCircle x -> x.Time
        | Hold x -> x.Time
        | Slider x -> x.Time
        | Spinner x -> x.Time

    static member inline CreateManiaNote(keys: int, key: int, time: ^T) : HitObject =
        HitCircle.Create(keys, key, time) |> HitCircle

    static member inline CreateManiaHold(keys: int, key: int, start_time: ^T, end_time: ^T) : HitObject =
        Hold.Create(keys, key, start_time, end_time) |> Hold

    override this.ToString() : string =
        match this with
        | HitCircle x -> x.ToString()
        | Hold x -> x.ToString()
        | Slider x -> x.ToString()
        | Spinner x -> x.ToString()

    static member FromString(line: string) : HitObject =
        let values = line.Split(',')

        if line.StartsWith(' ') then
            raise(ParseException([| line, "Hit object may not begin with a space" |]))

        let x =
            values
                .ValueAt(0)
                .ParseFloat()
                .RejectNan()
                .RejectInfinity()
                .ClampBetween(0f, 512f)
                .TruncateToInt()
                .ExpectValid(line)

        let y =
            values
                .ValueAt(1)
                .ParseFloat()
                .RejectNan()
                .RejectInfinity()
                .ClampBetween(0f, 512f)
                .TruncateToInt()
                .ExpectValid(line)

        let time =
            values
                .ValueAt(2)
                .ParseFloat()
                .RejectOutOfRange(0.0f, infinityf)
                .RejectNan()
                .RejectInfinity()
                .TruncateToInt()
                .ExpectValid(line)

        let obj_type = values.ValueAt(3).ParseInt().ExpectValid(line)
        let hitsound = values.ValueAt(4).ParseInt().ExpectValid(line) &&& 14 |> enum

        let starts_new_combo = obj_type &&& 4 <> 0
        let color_hax = (obj_type >>> 4) &&& 7

        let inline parse_hitsample (index: int) =
            HitSample.FromString(values.ValueAt(index).DefaultValue("0:0:0:0:"))

        let inline parse_hitcircle () =
            HitCircle
                {
                    X = x
                    Y = y
                    Time = time
                    StartsNewCombo = starts_new_combo
                    ColorHax = color_hax
                    HitSound = hitsound
                    HitSample = parse_hitsample(5)
                }

        let inline parse_slider () =

            let curve_parts = values.ValueAt(5).DefaultValue("").Split('|', 2)
            let curve_shape = SliderShape.FromString(curve_parts.ValueAt(0).DefaultValue(""))

            let curve_points =

                let inline curve_point (point: string) =
                    let split = point.Split(':')

                    let inline coord (index: int) =
                        split
                            .ValueAt(index)
                            .ParseFloat()
                            .ReplaceNanWith(float32 Int32.MinValue)
                            .ReplaceOutOfRangeInclusiveWith(
                                float32 Int32.MinValue,
                                float32 Int32.MaxValue,
                                float32 Int32.MinValue
                            )
                            .TruncateToInt()
                            .ExpectValid(line, curve_parts.[1])

                    coord(0), coord(1)

                if curve_parts.Length < 2 then [] else curve_parts.[1].Split('|') |> Seq.map curve_point |> List.ofSeq

            let slides =
                values.ValueAt(6).ParseInt().ClampBetween(1, 9001).RejectOutOfRange(1, 9000).ExpectValid(line)

            let inline edge_sounds (slides: int) : HitSound list =
                let pipe_separated_sounds = values.ValueAt(8).DefaultValue("").Split('|')

                let inline edge_sound (index: int) : HitSound =
                    pipe_separated_sounds.ValueAt(index).ParseInt().DefaultValue(0) |> enum

                seq { 0..slides } |> Seq.map edge_sound |> List.ofSeq

            let inline edge_sets (slides: int) : (SampleSet * SampleSet) list =
                let pipe_separated_sets =
                    values.ValueAt(9).ReplaceWhitespaceWith("0:0").DefaultValue("0:0").Split('|')

                let inline edge_set_pair (index: int) : SampleSet * SampleSet =
                    let split = pipe_separated_sets.ValueAt(index).DefaultValue("0:0").Split(':')

                    split.ValueAt(0).ParseInt().ExpectValid(line) |> enum,
                    split.ValueAt(1).ParseInt().ExpectValid(line) |> enum

                let inline other_pairs_must_parse () =
                    seq { slides + 1 .. pipe_separated_sets.Length - 1 }
                    |> Seq.map edge_set_pair
                    |> Seq.length
                    |> ignore

                let inline pairs_used_by_slides () =
                    seq { 0..slides } |> Seq.map edge_set_pair |> List.ofSeq

                other_pairs_must_parse()
                pairs_used_by_slides()

            Slider
                {
                    X = x
                    Y = y
                    Time = time
                    StartsNewCombo = starts_new_combo
                    ColorHax = color_hax
                    HitSound = hitsound
                    CurveType = curve_shape
                    CurvePoints = curve_points
                    Slides = slides
                    Length =
                        values
                            .ValueAt(7)
                            .ParseDouble()
                            .ReplaceZeroWith(70.0)
                            .RejectOutOfRange(0.0, 1_000_000.0)
                            .ExpectValid(line)
                    EdgeSounds = edge_sounds(slides)
                    EdgeSets = edge_sets(slides)
                    HitSample = parse_hitsample(10)
                }

        let inline parse_spinner () =
            Spinner
                {
                    X = x
                    Y = y
                    Time = time
                    StartsNewCombo = starts_new_combo
                    ColorHax = color_hax
                    HitSound = hitsound
                    EndTime =
                        values
                            .ValueAt(5)
                            .ParseFloat()
                            .ReplaceNanWith(float32 Int32.MinValue)
                            .ReplaceOutOfRangeWith(
                                float32 Int32.MinValue,
                                float32 Int32.MaxValue,
                                float32 Int32.MinValue
                            )
                            .TruncateToInt()
                            .ExpectValid(line)
                    HitSample = parse_hitsample(6)
                }

        let inline parse_hold () =
            let endtime_and_sample =
                values.ValueAt(5).ExpectValid(line).Split(':', 2, StringSplitOptions.TrimEntries)

            Hold
                {
                    X = x
                    Y = y
                    Time = time
                    StartsNewCombo = starts_new_combo
                    ColorHax = color_hax
                    HitSound = hitsound
                    EndTime =
                        endtime_and_sample
                            .ValueAt(0)
                            .ParseFloat()
                            .ReplaceNanWith(float32 Int32.MinValue)
                            .ReplaceOutOfRangeWith(
                                float32 Int32.MinValue,
                                float32 Int32.MaxValue,
                                float32 Int32.MinValue
                            )
                            .TruncateToInt()
                            .ExpectValid(line)
                    HitSample = HitSample.FromString(endtime_and_sample.ValueAt(1).DefaultValue("0:0:0:0:"))
                }

        if obj_type &&& 1 > 0 then parse_hitcircle()
        elif obj_type &&& 2 > 0 then parse_slider()
        elif obj_type &&& 8 > 0 then parse_spinner()
        elif obj_type &&& 128 > 0 then parse_hold()
        else raise(ParseException([| line, "Unrecognised object type" |]))
