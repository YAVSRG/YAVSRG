using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts.Osu
{
    class StoryboardEvent
    {
        //i didn't properly implement this because this was a quick bit of code to get the bg image
        public string[] data;

        public StoryboardEvent(string l)
        {
            string[] split = l.Split(',');
            data = split;
        }
    }
}
