using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.Osu
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

            x = int.Parse(parts[0]);
            y = int.Parse(parts[1]);
            offset = float.Parse(parts[2]);
            type = int.Parse(parts[3]);
            hitsound = int.Parse(parts[4]);
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
