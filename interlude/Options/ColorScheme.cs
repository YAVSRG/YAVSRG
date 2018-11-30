using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Options
{
    public class ColorScheme
    {
        public Colorizer.ColorStyle Style;
        public int[,] ColorMap;
        public bool UseForAllKeyModes = true;
        public bool LNEndsMatchBody = true;

        public ColorScheme(Colorizer.ColorStyle s)
        {
            Style = s;
            ColorMap = new int[10, 9];
        }

        public int GetColorIndex(int i, int k)
        {
            k = UseForAllKeyModes ? 0 : k - 2;
            if (i < ColorMap.Length)
            {
                return ColorMap[i, k];
            }
            return 0;
        }

        public void SetColorIndex(int i, int k, int v)
        {
            k = k >= 3 ? k - 2 : 0;
            ColorMap[i, k] = v;
        }

        public int GetColorCount(int keys)
        {
            switch (Style)
            {
                case Colorizer.ColorStyle.DDR:
                case Colorizer.ColorStyle.Jackhammer:
                    return Colorizer.DDRValues.Length + 1;
                case Colorizer.ColorStyle.Chord:
                default:
                    return keys;
            }
        }

        public string GetDescription(int i)
        {
            switch (Style)
            {
                case Colorizer.ColorStyle.DDR:
                    return i == Colorizer.DDRValues.Length ? "Color for unsnapped notes" : "Color for notes landing on every 1/" + Colorizer.DDRValues[i].ToString() + " of a beat";
                case Colorizer.ColorStyle.Chord:
                    return i > 0 ? "Color for a chord of " + (i + 1).ToString() + " notes" : "Color for single notes";
                case Colorizer.ColorStyle.Column:
                    return "Color for notes in column " + (i + 1).ToString();
                default:
                    return "";
            }
        }
    }
}
