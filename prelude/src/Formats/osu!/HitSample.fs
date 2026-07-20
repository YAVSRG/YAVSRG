namespace Prelude.Formats.Osu

type HitSample =
    {
        NormalSet: SampleSet
        AdditionSet: SampleSet
        Index: int
        Volume: int
        Filename: string
    }
    
    static member Default: HitSample =
        {
            NormalSet = SampleSet.None
            AdditionSet = SampleSet.None
            Index = 0
            Volume = 0
            Filename = ""
        }
        
    override this.ToString() : string =
        sprintf "%i:%i:%i:%i:%s" (int this.NormalSet) (int this.AdditionSet) this.Index this.Volume this.Filename
        
    static member FromString(sample: string) : HitSample =
        let values = SplitValues.Parse(sample, ':')
        {
            NormalSet = values.EnumOrDefault(0, SampleSet.Default, true)
            AdditionSet = values.EnumOrDefault(1, SampleSet.Default, true)
            Index = values.EnumOrDefault(2, SampleSet.Default, true) |> int
            Volume = values.EnumOrDefault(3, SampleSet.Default, true) |> int |> max 0 |> min 100
            Filename = values.UntrimmedStringOrDefault(4, "")
        }
