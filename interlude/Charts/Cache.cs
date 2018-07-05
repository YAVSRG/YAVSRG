using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;
using System.IO;

namespace YAVSRG.Charts
{
    public class Cache
    {
        [Newtonsoft.Json.JsonIgnore]
        static readonly string CacheVersion = "1.1";

        static string GetCachePath()
        {
            return Path.Combine(Game.WorkingDirectory, "Data", "Cache.json");
        }

        public static Cache LoadCache()
        {
            string path = GetCachePath();
            if (File.Exists(path))
            {
                Cache c = Utils.LoadObject<Cache>(path);
                if (c.Version == CacheVersion)
                {
                    return c;
                }
            }
            return new Cache();
        }

        public Dictionary<string, CachedChart> Charts = new Dictionary<string, CachedChart>();
        public string Version = CacheVersion;
        
        public Chart LoadChart(CachedChart c)
        {
            string path = Path.Combine(c.abspath, c.file);
            Chart m = Chart.FromFile(path); //could be null
            if (m != null)
            {
                CacheChart(m);
            }
            return m;
        }

        public void CacheChart(Chart c)
        {
            string id = Path.Combine(c.Data.SourcePath, c.Data.File);
            Charts[id] = CachedChart.FromChart(c);
            //if (Game.Gameplay.ScoreDatabase.data.ContainsKey(Charts[id].hash))
            //{
            //    Game.Gameplay.ScoreDatabase.GetChartSaveData(c).Path = id;
            //}
        }

        public void Save()
        {
            Utils.SaveObject(this, GetCachePath());
        }
    }
}
