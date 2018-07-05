using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;
using System.IO;

namespace YAVSRG.Gameplay
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
            /*foreach (string s in Directory.GetFiles(Path.Combine(Content.WorkingDirectory, "Data", "Scores")))
            {
                var o = Utils.LoadObject<ChartSaveData>(s);
                if (o != null && o.Scores.Count > 0)
                {
                    x.data.Add(Path.GetFileNameWithoutExtension(s), o);
                }
            }*/
            return x;
        }

        public void Save()
        {
            string path = Path.Combine(Game.WorkingDirectory, "Data", "Scores.json");
            Utils.SaveObject(this, path);
        }
    }
}
