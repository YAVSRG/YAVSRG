using System.Collections.Generic;
using System.IO;

namespace Prelude.Gameplay.Charts.Osu
{
    public class EventData //this is the block that stores storyboard events. one of such is what bg image to use
    {
        private List<StoryboardEvent> points;

        public EventData(TextReader fs) //reads from text file
        {
            points = new List<StoryboardEvent>();
            string l;
            while (true)
            {
                l = fs.ReadLine();
                if (l == "") return;
                if (l.StartsWith("//")) continue; //ignore comments
                points.Add(new StoryboardEvent(l)); //parses storyboard event
            }
        }

        public string GetBGPath() //just rudimentary hack to locate bg path from data
        {
            foreach (StoryboardEvent s in points)
            {
                if (s.data[0] == "0") //0 means bg event
                {
                    return s.data[2].Trim('"');
                }
            }
            return "";
        }

        public void Dump(TextWriter tw)
        {
            //stub. will write to text file
        }
    }
}
