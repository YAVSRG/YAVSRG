namespace Prelude.Charts.Formats.osu

open System.Globalization

type TimingEffect =
    | None = 0
    | Kiai = 1
    | OmitFirstBarline = 8

type UninheritedTimingPoint =
    {
        Time: int
        MsPerBeat: float
        Meter: int
        SampleSet: SampleSet
        SampleIndex: int
        Volume: int
        Effects: TimingEffect
    }
    override this.ToString() =
        sprintf "%i,%s,%i,%i,%i,%i,1,%i"
            this.Time
            (this.MsPerBeat.ToString(CultureInfo.InvariantCulture))
            this.Meter
            (int this.SampleSet)
            this.SampleIndex
            this.Volume
            (int this.Effects)

type InheritedTimingPoint =
    {
        Time: int
        Multiplier: float
        METER__UNUSED: int
        SampleSet: SampleSet
        SampleIndex: int
        Volume: int
        Effects: TimingEffect
    }
    override this.ToString() =
        sprintf "%i,%s,%i,%i,%i,%i,0,%i"
            this.Time
            (-100.0 / this.Multiplier |> fun f -> f.ToString(CultureInfo.InvariantCulture))
            this.METER__UNUSED
            (int this.SampleSet)
            this.SampleIndex
            this.Volume
            (int this.Effects)

type TimingPoint =
    | Uninherited of UninheritedTimingPoint
    | Inherited of InheritedTimingPoint
    member this.Time =
        match this with
        | Uninherited x -> x.Time
        | Inherited x -> x.Time
    override this.ToString() =
        match this with
        | Uninherited x -> x.ToString()
        | Inherited x -> x.ToString()