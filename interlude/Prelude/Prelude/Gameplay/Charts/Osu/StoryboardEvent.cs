using System;

namespace Prelude.Gameplay.Charts.Osu
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
