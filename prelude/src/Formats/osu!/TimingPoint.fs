namespace Prelude.Formats.Osu

open System.Globalization

type TimingEffect =
    | None = 0
    | Kiai = 1
    | OmitFirstBarline = 8

/// Represents a red line in the osu! editor
/// Note that `time` being a float is not a mistake, unlike hitobjects, osu! supports floats for the timestamp of timing points
type UninheritedTimingPoint =
    {
        Time: float
        MsPerBeat: float
        Meter: int
        SampleSet: SampleSet
        SampleIndex: int
        Volume: int
        Effects: TimingEffect
    }

    member this.KiaiMode = int this.Effects &&& int TimingEffect.Kiai <> 0

    override this.ToString() : string =
        sprintf
            "%s,%s,%i,%i,%i,%i,1,%i"
            (this.Time.ToString(CultureInfo.InvariantCulture))
            (this.MsPerBeat.ToString(CultureInfo.InvariantCulture))
            this.Meter
            (int this.SampleSet)
            this.SampleIndex
            this.Volume
            (int this.Effects)

    static member inline Create(time: ^X, ms_per_beat: ^Y, meter: ^Z) : UninheritedTimingPoint =
        {
            Time = float time
            MsPerBeat = float ms_per_beat |> abs
            Meter = int meter
            SampleSet = SampleSet.Soft
            SampleIndex = 0
            Volume = 10
            Effects = TimingEffect.None
        }

/// Represents a green line in the osu! editor
/// Note that `time` being a float is not a mistake, unlike hitobjects, osu! supports floats for the timestamp of timing points
type InheritedTimingPoint =
    {
        Time: float
        Multiplier: float
        SampleSet: SampleSet
        SampleIndex: int
        Volume: int
        Effects: TimingEffect
    }

    member this.KiaiMode = int this.Effects &&& int TimingEffect.Kiai <> 0

    member this.OmitFirstBarLine =
        int this.Effects &&& int TimingEffect.OmitFirstBarline <> 0

    override this.ToString() : string =
        sprintf
            "%s,%s,4,%i,%i,%i,0,%i"
            (this.Time.ToString(CultureInfo.InvariantCulture))
            ((-100.0 / this.Multiplier).ToString(CultureInfo.InvariantCulture))
            (int this.SampleSet)
            this.SampleIndex
            this.Volume
            (int this.Effects)

    static member inline Create(time: ^Z, multiplier: ^Y) : InheritedTimingPoint =
        {
            Time = float time
            Multiplier = float multiplier |> min 100.0 |> max 0.01
            SampleSet = SampleSet.Soft
            SampleIndex = 0
            Volume = 10
            Effects = TimingEffect.None
        }

type TimingPoint =
    | Uninherited of UninheritedTimingPoint
    | Inherited of InheritedTimingPoint

    member this.Time =
        match this with
        | Uninherited x -> x.Time
        | Inherited x -> x.Time

    static member inline CreateBPM(time, ms_per_beat, meter) : TimingPoint =
        UninheritedTimingPoint.Create(time, ms_per_beat, meter) |> Uninherited

    static member inline CreateSV(time, multiplier) : TimingPoint =
        InheritedTimingPoint.Create(time, multiplier) |> Inherited

    override this.ToString() : string =
        match this with
        | Uninherited x -> x.ToString()
        | Inherited x -> x.ToString()

    static member FromString(line: string) : TimingPoint =
        let values = line.Split(',')

        if values.Length = 0 then
            failwith "Invalid timing point: empty line"
        elif values.Length = 3 then
            failwithf "Invalid timing point, osu! accepts either 2 values or 4+: %s" line

        let time =
            values.ValueAt(0).ParseDouble().RejectNan().RejectInfinity().ExpectValid(line)

        let meter =
            values.ValueAt(2).ReplaceInvalidWith("4").ParseInt().ReplaceZeroWith(4).ExpectValid(line)

        let sample_set =
            values.ValueAt(3).ReplaceInvalidWith("2").ParseInt().ReplaceZeroWith(2).ExpectValid(line) |> enum

        let sample_index =
            values.ValueAt(4).ReplaceInvalidWith("0").ParseInt().ExpectValid(line)

        let volume =
            values.ValueAt(5).ReplaceInvalidWith("100").ParseInt().ClampBetween(1, 100).ExpectValid(line)

        let effects =
            values.ValueAt(7).ReplaceInvalidWith("0").ParseInt().ExpectValid(line) |> enum

        let inline parse_uninherited () =
            Uninherited
                {
                    Time = time
                    MsPerBeat =
                        values
                            .ValueAt(1)
                            .ParseDouble()
                            .RejectNan()
                            .ReplaceZeroWith(infinity)
                            .RejectInfinity()
                            .ExpectValid(line)
                    Meter = meter
                    SampleSet = sample_set
                    SampleIndex = sample_index
                    Volume = volume
                    Effects = effects
                }

        let inline parse_inherited () =
            Inherited
                {
                    Time = time
                    Multiplier =
                        let v = values.ValueAt(1).ParseDouble().RejectInfinity().ExpectValid(line)
                        if v >= 0 || System.Double.IsNaN(v) then 1.0 else -100.0 / v |> min 100.0 |> max 0.01
                    SampleSet = sample_set
                    SampleIndex = sample_index
                    Volume = volume
                    Effects = effects
                }

        let is_uninherited = values.ValueAt(6).DefaultValue("1").StartsWith('1')
        if is_uninherited then parse_uninherited() else parse_inherited()
