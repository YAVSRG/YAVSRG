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
        let values = sample.Split(':')

        {
            NormalSet = values.ValueAt(0).ParseInt().ExpectValid(sample, 0) |> enum
            AdditionSet = values.ValueAt(1).ParseInt().ExpectValid(sample, 1) |> enum
            Index = values.ValueAt(2).ParseInt().ExpectValid(sample, 2)
            Volume = values.ValueAt(3).ParseInt().ClampBetween(0, 100).ExpectValid(sample, 3)
            Filename = values.ValueAt(4).ExpectValid(sample, 4)
        }
