namespace Prelude.Charts.Formats.osu

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

type HitObject =
    | HitCircle of HitCircle
    | HoldNote of Hold
    | Slider of Slider
    | Spinner of Spinner
    member this.Time =
        match this with
        | HitCircle x -> x.Time
        | HoldNote x -> x.Time
        | Slider x -> x.Time
        | Spinner x -> x.Time
    override this.ToString() =
        match this with
        | HitCircle x -> x.ToString()
        | HoldNote x -> x.ToString()
        | Slider x -> x.ToString()
        | Spinner x -> x.ToString()