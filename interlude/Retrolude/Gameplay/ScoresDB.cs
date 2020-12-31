using System.Collections.Generic;
using System.IO;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Interlude.Gameplay
{
    public class ScoresDB
    {
        [Newtonsoft.Json.JsonProperty]
        public Dictionary<string, ChartSaveData> data;

        public ChartSaveData GetChartSaveData(Chart c)
        {
            string hash = c.GetHash();
            if (!data.ContainsKey(hash))
            {
                data.Add(hash, ChartSaveData.FromChart(c));
            }
            return data[hash];
        }

        public static ScoresDB Load()
        {
            string path = Path.Combine(Game.WorkingDirectory, "Data", "Scores.json");
            if (File.Exists(path))
            {
                return Utils.LoadObject<ScoresDB>(path);
            }
            var x = new ScoresDB() { data = new Dictionary<string, ChartSaveData>() };
            return x;
        }

        public void Save()
        {
            string path = Path.Combine(Game.WorkingDirectory, "Data", "Scores.json");
            Utils.SaveObject(this, path);
        }
    }
}
