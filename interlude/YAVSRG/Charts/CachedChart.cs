using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Charts
{
    public class CachedChart
    {
        public string file;
        public string title;
        public string artist;
        public string creator;
        public string abspath;
        public string pack;
        public int keymode;
        public float length;
        public int bpm;
        public string diffname;
        public float physical;
        public float technical;

        public static CachedChart FromChart(YAVSRG.Chart c)
        {
            DifficultyRating.RatingReport r = new DifficultyRating.RatingReport(new Gameplay.ChartWithModifiers(c), 1.0f);
            return new CachedChart
            {
                file = c.Data.File,
                title = c.Data.Title,
                artist = c.Data.Artist,
                creator = c.Data.Creator,
                abspath = c.Data.SourcePath,
                pack = c.Data.SourcePack,
                keymode = c.Keys,
                length = c.GetDuration(),
                bpm = c.GetBPM(),
                diffname = c.Data.DiffName,
                physical = r.breakdown[0],
                technical = r.breakdown[1]
            };
        }
    }
}

