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
            NormalSet = values.EnumOrDefault(0, SampleSet.Default)
            AdditionSet = values.EnumOrDefault(1, SampleSet.Default)
            Index = values.IntOrDefault(2, 0)
            Volume = values.IntOrDefault(3, 0)
            Filename = values.StringOrDefault(4, "")
        }
