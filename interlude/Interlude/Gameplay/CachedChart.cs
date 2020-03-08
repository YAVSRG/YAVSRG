using Newtonsoft.Json;
using Prelude.Gameplay.DifficultyRating;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay;

namespace Interlude.Gameplay
{
    public class CachedChart
    {
        public string file;
        public string title;
        public string artist;
        public string creator;
        public string abspath;
        public string pack;
        public string hash;
        public int keymode;
        public float length;
        public int bpm;
        public string diffname;
        public float physical;
        public float technical;

        [JsonIgnore]
        public string collection;
        [JsonIgnore]
        public int collectionIndex;

        public static CachedChart FromChart(Chart c)
        {
            RatingReport r = new RatingReport(new ChartWithModifiers(c), 1, KeyLayout.Layout.Spread);
            return new CachedChart
            {
                file = c.Data.File,
                title = c.Data.Title,
                artist = c.Data.Artist,
                creator = c.Data.Creator,
                abspath = c.Data.SourcePath,
                pack = c.Data.SourcePack,
                hash = c.GetHash(),
                keymode = c.Keys,
                length = c.GetDuration(),
                bpm = c.AverageBPM(),
                diffname = c.Data.DiffName,
                physical = r.Physical,
                technical = r.Technical
            };
        }

        public string GetFileIdentifier()
        {
            return System.IO.Path.Combine(abspath, file);
        }

        public CachedChart Rename_This_Hack(string collection, int index)
        {
            CachedChart result = (CachedChart)MemberwiseClone();
            result.collection = collection;
            result.collectionIndex = index;
            return result;
        }
    }
}

