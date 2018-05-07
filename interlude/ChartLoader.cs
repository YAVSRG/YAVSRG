using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap;
using YAVSRG.Beatmap.Stepmania;
using System.IO;

namespace YAVSRG
{
    public class ChartLoader
    {
        public static Func<CachedChart,string> GroupByPack = (c) => { return c.pack; };
        public static Func<CachedChart, string> GroupByTitle = (c) => { return Utils.FormatFirstCharacter(c.title); };
        public static Func<CachedChart, string> GroupByDifficulty = (c) => { return "NYI"; };
        public static Func<CachedChart, string> GroupByCreator = (c) => { return Utils.FormatFirstCharacter(c.creator); };
        public static Func<CachedChart, string> GroupByArtist = (c) => { return Utils.FormatFirstCharacter(c.artist); };

        public static Comparison<CachedChart> SortByDifficulty = (a,b) => (0.CompareTo(0));
        public static Comparison<CachedChart> SortByTitle = (a, b) => (a.title.CompareTo(b.title));
        public static Comparison<CachedChart> SortByCreator = (a, b) => (a.creator.CompareTo(b.creator));
        public static Comparison<CachedChart> SortByArtist = (a, b) => (a.artist.CompareTo(b.artist));

        public static Comparison<CachedChart> SortMode = SortByTitle;
        public static Func<CachedChart,string> GroupMode = GroupByPack;
        public static string SearchString = "";
        public static event Action OnRefreshGroups = () => { };
        public static bool Loaded;
        public static List<ChartGroup> Groups;
        public static List<ChartGroup> SearchResult;
        public static List<CachedChart> Cache;
        public static MultiChart SelectedChart;

        static readonly string CacheVersion = "v1.1";

        public class ChartGroup
        {
            public List<CachedChart> charts;
            public string label;

            public ChartGroup(List<CachedChart> charts, string label)
            {
                this.charts = charts;
                this.label = label;
            }

            public void Sort(Comparison<CachedChart> comp, bool reverse)
            {
                charts.Sort(comp);
                if (reverse) { charts.Reverse(); }
            }
        }

        public struct CachedChart
        {
            public string version;
            public string title;
            public string artist;
            public string creator;
            public string abspath;
            public string pack;
            public string[] keys;
        }


        public static void Init()
        {
            Groups = new List<ChartGroup>();
            LoadCache();
            if (Cache.Count == 0)
            {
                UpdateCache();
            }
            Refresh();
            Loaded = true;
        }

        public static void SortIntoGroups(Func<CachedChart,string> groupBy, Comparison<CachedChart> sortBy)
        {
            Groups = new List<ChartGroup>();
            Dictionary<string, List<CachedChart>> temp = new Dictionary<string, List<CachedChart>>();
            string s;
            foreach (CachedChart c in Cache)
            {
                s = groupBy(c);
                if (temp.ContainsKey(s))
                {
                    temp[s].Add(c);
                }
                else
                {
                    temp.Add(s, new List<CachedChart> { c });
                }
            }
            foreach (string k in temp.Keys)
            {
                ChartGroup g = new ChartGroup(temp[k], k);
                g.Sort(sortBy, false);
                Groups.Add(g);
            }
            Groups.Sort((a, b) => { return a.label.CompareTo(b.label); });
        }

        public static void SearchGroups(string s)
        {
            s = s.ToLower();
            SearchResult = new List<ChartGroup>();
            bool keymodeMatch;
            foreach (ChartGroup g in Groups)
            {
                List<CachedChart> temp = new List<CachedChart>();
                foreach (CachedChart c in g.charts)
                {
                    keymodeMatch = Game.Options.Profile.Keymode == 0 || c.keys.Contains(Game.Options.Profile.Keymode.ToString());
                    if (keymodeMatch && (c.title.ToLower().Contains(s) || c.creator.ToLower().Contains(s) || c.artist.ToLower().Contains(s)))
                    {
                        temp.Add(c);
                    }
                }
                if (temp.Count > 0)
                {
                    SearchResult.Add(new ChartGroup(temp, g.label + " (" + temp.Count.ToString() + ")"));
                }
            }
        }

        public static void Refresh()
        {
            SortIntoGroups(GroupMode, SortMode);
            SearchGroups(SearchString);
            OnRefreshGroups();
        }

        public static void RandomChart()
        {
            if (Cache.Count == 0) {
                SelectedChart = new MultiChart(new ChartHeader { pack = "Nowhere", artist = "Percyqaz", creator = "Nobody", title = "You have no songs installed!", path = Content.WorkingDirectory });
                Game.Gameplay.ChangeChart(new Chart(new List<Snap>(), new List<BPMPoint>(), "Default", 0, 4, Content.WorkingDirectory, "", ""));
                return;
            }
            SelectedChart = LoadFromCache(Cache[new Random().Next(0, Cache.Count)]);
            Game.Gameplay.ChangeChart(SelectedChart.diffs[new Random().Next(0, SelectedChart.diffs.Count)]);
        }

        public static void LoadCache()
        {
            Cache = new List<CachedChart>();
            CachedChart c;
            foreach (string s in Directory.GetFiles(Path.Combine(Content.WorkingDirectory, "Data", "Cache")))
            {
                c = LoadCacheFile(s);
                if (!Directory.Exists(c.abspath))
                {
                    continue;
                }
                if (c.version != CacheVersion) //update this as i go
                {
                    continue;
                }
                Cache.Add(c);
            }
        }

        public static void UpdateCache()
        {
            Cache = new List<CachedChart>();
            foreach (string s in Directory.GetDirectories(Path.Combine(Content.WorkingDirectory, "Songs")))
            {
                CachePack(s, Path.GetFileName(s));
            }
            if (Directory.Exists(GetOsuSongFolder()))
            {
                CachePack(GetOsuSongFolder(), "osu! Imports");
            }
        }

        public static void UpdateCacheThreaded()
        {
            Loaded = false;
            System.Threading.Thread t = new System.Threading.Thread(new System.Threading.ThreadStart(() => { UpdateCache(); Loaded = true; }));
            t.Start();
        }

        private static CachedChart LoadCacheFile(string path)
        {
            FileStream fs = new FileStream(path, FileMode.Open);
            TextReader t = new StreamReader(fs);
            string version = t.ReadLine();
            CachedChart c;
            if (version != CacheVersion) { c = new CachedChart { version = "" }; }
            else
            {
                c = new CachedChart {
                    version = CacheVersion,
                    title = t.ReadLine(), artist = t.ReadLine(), creator = t.ReadLine(), abspath = t.ReadLine(), pack = t.ReadLine(), keys = t.ReadLine().Split(';')
                }; //implement length and difficulty caching
            }
            t.Close();
            fs.Close();
            return c;
        }

        private static void SaveCacheFile(string path, CachedChart c)
        {
            FileStream fs = new FileStream(path, FileMode.Create);
            TextWriter t = new StreamWriter(fs);
            t.WriteLine(CacheVersion);
            t.WriteLine(c.title); //could probably stand to make this neater
            t.WriteLine(c.artist);
            t.WriteLine(c.creator);
            t.WriteLine(c.abspath);
            t.WriteLine(c.pack);
            t.WriteLine(string.Join(";",c.keys));
            t.Close();
            fs.Close();
        }

        private static void CachePack(string folder, string packname)
        {
            CachedChart c;
            string path;
            foreach (string s in Directory.GetDirectories(folder))
            {
                path = Path.Combine(Content.WorkingDirectory,"Data","Cache",packname+Path.GetFileNameWithoutExtension(s)+".cache");
                if (File.Exists(path) && true) //replace true with verification that file edit date is older than cache edit date
                {
                    c = LoadCacheFile(path);
                    if (c.version != CacheVersion) //update this as i go
                    {
                        try
                        {
                            c = CacheChart(LoadFromPath(s, packname));
                            SaveCacheFile(path, c);
                        }
                        catch
                        {
                            continue;
                        }
                    }
                }
                else
                {
                    try
                    {
                        c = CacheChart(LoadFromPath(s,packname));
                        SaveCacheFile(path, c);
                    }
                    catch
                    {
                        //normally when no difficulties are loadable
                        continue;
                    }
                }
                Cache.Add(c);
            }
        }

        public static MultiChart LoadFromPath(string p, string packname)
        {
            MultiChart c = null;
            Chart d = null;
            foreach (string f in Directory.GetFiles(p))
            {
                if (Path.GetExtension(f) == ".osu")
                {
                    OsuBeatmap osu = new OsuBeatmap(Path.GetFileName(f), Path.GetDirectoryName(f));
                    if (c == null)
                    {
                        c = osu.ConvertToRoot();
                    }
                    else
                    {
                        d = osu.Convert();
                        if (d != null)
                        {
                            c.diffs.Add(d);
                        }
                    }
                }
                else if (Path.GetExtension(f) == ".sm")
                {
                    StepFile sm = new StepFile(Path.GetFileName(f), Path.GetDirectoryName(f));
                    if (c == null)
                    {
                        c = sm.ConvertToRoot();
                    }
                    else
                    {
                        foreach (Chart x in sm.Convert())
                        {
                            c.diffs.Add(x);
                        }
                    }
                }
            }
            c.header.pack = packname;
            return c;
        }

        public static string GetOsuSongFolder()
        {
            return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData),"osu!","Songs");
        }

        private static CachedChart CacheChart(MultiChart c)
        {
            List<string> keys = new List<string>();
            foreach (Chart chart in c.diffs)
            {
                if (!keys.Contains(chart.Keys.ToString()))
                {
                    keys.Add(chart.Keys.ToString());
                }
            }
            return new CachedChart
            {
                title = c.header.title,
                artist = c.header.artist,
                creator = c.header.creator,
                abspath = c.header.path,
                pack = c.header.pack,
                keys = keys.ToArray()
            };
        }

        public static MultiChart LoadFromCache(CachedChart c)
        {
            return LoadFromPath(c.abspath, c.pack);
        }
    }
}
