using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap
{
    public struct ChartHeader
    {
        public string title;
        public string artist;
        public string creator;
        public string path;

        public ChartHeader(string t, string a, string c, string p)
        {
            title = t; artist = a; creator = c; path = p;
        }
    }
}
