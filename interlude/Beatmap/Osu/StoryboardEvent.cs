using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap.Osu
{
    class StoryboardEvent
    {
        public string[] data;

        public StoryboardEvent(string l)
        {
            string[] split = l.Split(',');
            data = split;
        }
    }
}
