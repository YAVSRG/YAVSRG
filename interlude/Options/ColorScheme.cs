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
                    return Colorizer.DDRValues.Length;
                case Colorizer.ColorStyle.Chord:
                case Colorizer.ColorStyle.Column:
                default:
                    return keys;
            }
        }
    }
}
