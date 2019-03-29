using System.Globalization;

namespace Prelude.Gameplay.Charts.Osu
{
    public class HitObject //represents a hit object
    {
        //https://osu.ppy.sh/help/wiki/osu!_File_Formats/Osu_(file_format)

        public int x, y;
        public float offset;
        public int type;
        public int hitsound;
        public string addition;

        public HitObject(int x, int y, float offset, int type, int hitsound, string addition)
        {
            this.x = x;
            this.y = y;
            this.offset = offset;
            this.type = type;
            this.hitsound = hitsound;
            this.addition = addition;
        }

        public HitObject(string parse) //splits line into parts and interprets it
        {
            string[] parts = parse.Split(',');

            x = int.Parse(parts[0], CultureInfo.InvariantCulture);
            y = int.Parse(parts[1], CultureInfo.InvariantCulture);
            offset = float.Parse(parts[2], CultureInfo.InvariantCulture);
            type = int.Parse(parts[3], CultureInfo.InvariantCulture);
            hitsound = int.Parse(parts[4], CultureInfo.InvariantCulture);
            if (parts.Length > 5)
            {
                addition = parts[5];
            }
        }

        public void Dump(System.IO.TextWriter tw) //writes to text file
        {
            tw.WriteLine(x.ToString() + "," + y.ToString() + "," + offset.ToString() + "," + type.ToString() + "," + hitsound.ToString() + "," + addition);
        }
    }
}
