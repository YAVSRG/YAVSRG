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
        sprintf "%s,%s,%i,%i,%i,%i,1,%i"
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
            MsPerBeat = float ms_per_beat |> max 0.0
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
    
    member this.OmitFirstBarLine = int this.Effects &&& int TimingEffect.OmitFirstBarline <> 0
    
    override this.ToString() : string =
        sprintf "%s,%s,4,%i,%i,%i,0,%i"
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
        
        let inline parse_uninherited(values: SplitValues) =
            Uninherited {
                Time = values.Float(0)
                MsPerBeat =
                    let v = values.FloatOrDefault(1, 500.0, true)
                    if v <= 0 || System.Double.IsNaN(v) then 500.0 else max 0.0 v
                Meter = values.IntOrDefault(2, 4) |> fun v -> if v <= 0 then 4 else v
                SampleSet = values.EnumOrDefault(3, SampleSet.Default, false)
                SampleIndex = values.IntOrDefault(4, 0)
                Volume = values.IntOrDefault(5, 100) |> max 0 |> min 100
                Effects = values.EnumOrDefault(7, TimingEffect.None, true)
            }
            
        let inline parse_inherited(values: SplitValues) =
            Inherited {
                Time = values.Float(0)
                Multiplier =
                    let v = values.FloatOrDefault(1, 1.0, true)
                    if v >= 0 || System.Double.IsNaN(v) then 1.0 else -100.0 / v |> min 100.0 |> max 0.01
                SampleSet = values.EnumOrDefault(3, SampleSet.Default, false)
                SampleIndex = values.IntOrDefault(4, 0)
                Volume = values.IntOrDefault(5, 100)
                Effects = values.EnumOrDefault(7, TimingEffect.None, true)
            }
            
        let values = SplitValues.Parse(line, ',')
        if values.Length = 0 then
            failwith "Invalid timing point: empty line"
        elif values.Length < 2 then
            failwithf "Invalid timing point, needs a comma separating 2+ values: %s" line
        else
            let is_uninherited = values.UntrimmedStringOrDefault(6, "1").StartsWith('1')
            if is_uninherited then parse_uninherited(values)
            else parse_inherited(values)