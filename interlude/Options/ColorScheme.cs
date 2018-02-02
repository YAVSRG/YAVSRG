using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Options
{
    class ColorScheme
    {
        public Colorizer.ColorStyle Style;
        public int[] ColorMap;

        public ColorScheme(Colorizer.ColorStyle s)
        {
            Style = s;
            ColorMap = new int[10]; //temp
        }

        public int GetColorIndex(int i)
        {
            if (i < ColorMap.Length)
            {
                return ColorMap[i];
            }
            return 0;
        }
    }
}
