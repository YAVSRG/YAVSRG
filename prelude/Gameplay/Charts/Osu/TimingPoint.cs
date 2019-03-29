using System.Globalization;

namespace Prelude.Gameplay.Charts.Osu
{
    public class TimingPoint
    {
        //https://osu.ppy.sh/help/wiki/osu!_File_Formats/Osu_(file_format)
        //see also: HitObject, which is commented

        public float offset;
        public float msPerBeat;
        public int meter, sampleType, sampleSet, volume;
        public bool inherited, kiai;

        public TimingPoint(float offset, float msPerBeat, int meter, int sampleType, int sampleSet, int volume, bool inherited, bool kiai)
        {
            this.offset = offset;
            this.msPerBeat = msPerBeat;
            this.meter = meter;
            this.sampleSet = sampleSet;
            this.sampleType = sampleType;
            this.volume = volume;
            this.inherited = inherited;
            this.kiai = kiai;
        }

        public TimingPoint(string parse)
        {
            string[] parts = parse.Split(',');

            offset = float.Parse(parts[0], CultureInfo.InvariantCulture);
            msPerBeat = (float)double.Parse(parts[1], CultureInfo.InvariantCulture);
            meter = int.Parse(parts[2], CultureInfo.InvariantCulture);
            sampleSet = int.Parse(parts[3], CultureInfo.InvariantCulture);
            sampleType = int.Parse(parts[4], CultureInfo.InvariantCulture);
            volume = int.Parse(parts[5], CultureInfo.InvariantCulture);
            inherited = int.Parse(parts[6], CultureInfo.InvariantCulture) == 0;
            kiai = int.Parse(parts[7], CultureInfo.InvariantCulture) == 1;
        }

        public float ScrollSpeed()
        {
            if (inherited)
            {
                return -100 / msPerBeat;
            }
            return 1f;
        }

        public TimingPoint GetChildPoint(float offset)
        {
            return new TimingPoint(offset, -100, meter, sampleSet, sampleSet, volume, true, kiai);
        }

        public void Dump(System.IO.TextWriter tw)
        {

        }
    }
}
