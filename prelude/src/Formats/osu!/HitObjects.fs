namespace Prelude.Formats.Osu

open System.Globalization

type HitSample =
    {
        NormalSet: SampleSet
        AdditionSet: SampleSet
        Index: int
        Volume: int
        Filename: string
    }
    static member Default =
        {
            NormalSet = SampleSet.None
            AdditionSet = SampleSet.None
            Index = 0
            Volume = 0
            Filename = ""
        }
    override this.ToString() =
        sprintf "%i:%i:%i:%i:%s" (int this.NormalSet) (int this.AdditionSet) this.Index this.Volume this.Filename

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
    override this.ToString() =
        sprintf "%i,%i,%i,%i,%i,%O"
            this.X
            this.Y
            this.Time
            (1 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.HitSample
    static member inline Create(keys: int, key: int, time: ^T) =
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
    override this.ToString() =
        match this with
        | Linear -> "L"
        | Catmull -> "C"
        | Bezier -> "B"
        | PerfectCircle -> "P"

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
    override this.ToString() =
        sprintf "%i,%i,%i,%i,%i,%O|%s,%i,%s,%s,%s,%O"
            this.X
            this.Y
            this.Time
            (2 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.CurveType
            (this.CurvePoints |> Seq.map (fun (x, y) -> sprintf "%i:%i" x y) |> String.concat "|")
            this.Slides
            (this.Length.ToString(CultureInfo.InvariantCulture))
            (this.EdgeSounds |> Seq.map (int >> sprintf "%i") |> String.concat "|")
            (this.EdgeSets |> Seq.map (fun (normal, addition) -> sprintf "%i:%i" (int normal) (int addition)) |> String.concat "|")
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
    override this.ToString() =
        sprintf "%i,%i,%i,%i,%i,%i,%O"
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
    override this.ToString() =
        sprintf "%i,%i,%i,%i,%i,%i:%O"
            this.X
            this.Y
            this.Time
            (128 ||| (if this.StartsNewCombo then 4 else 0) ||| (this.ColorHax &&& 7 <<< 4))
            (int this.HitSound)
            this.EndTime
            this.HitSample
    static member inline Create(keys: int, key: int, start_time: ^T, end_time: ^T) =
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
    override this.ToString() =
        match this with
        | HitCircle x -> x.ToString()
        | Hold x -> x.ToString()
        | Slider x -> x.ToString()
        | Spinner x -> x.ToString()
    static member inline CreateManiaNote(keys, key, time) = HitCircle.Create(keys, key, time) |> HitCircle
    static member inline CreateManiaHold(keys, key, start_time, end_time) = Hold.Create(keys, key, start_time, end_time) |> Hold