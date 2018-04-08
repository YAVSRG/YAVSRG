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
        public static Func<CachedChart, string> GroupByTitle = (c) => { return c.title.Substring(0,1).ToUpper(); };
        public static Func<CachedChart, string> GroupByDifficulty = (c) => { return "NYI"; };
        public static Func<CachedChart, string> GroupByCreator = (c) => { return c.creator; };
        public static Func<CachedChart, string> GroupByArtist = (c) => { return c.artist.Substring(0,1).ToUpper(); };

        public static Comparison<CachedChart> SortByDifficulty = (a,b) => (0.CompareTo(0));
        public static Comparison<CachedChart> SortByTitle = (a, b) => (a.title.CompareTo(b.title));
        public static Comparison<CachedChart> SortByCreator = (a, b) => (a.creator.CompareTo(b.creator));
        public static Comparison<CachedChart> SortByArtist = (a, b) => (a.artist.CompareTo(b.artist));

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
            public string title;
            public string artist;
            public string creator;
            public string abspath;
            public string pack;
            public string hash;
            public float[] difficulty;
        }

        public static bool Loaded;
        public static List<ChartGroup> Groups;
        public static List<CachedChart> Cache;
        public static MultiChart SelectedChart;

        public static void Init()
        {
            Groups = new List<ChartGroup>();
            Cache = new List<CachedChart>();
            UpdateCache();
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
        }

        public static void RandomChart()
        {
            SelectedChart = LoadFromCache(Cache[new Random().Next(0, Cache.Count)]);
            RandomDifficulty();
        }

        public static void RandomDifficulty()
        {
            Game.Gameplay.ChangeChart(SelectedChart.diffs[new Random().Next(0, SelectedChart.diffs.Count)]);
        }

        private static void LoadCache()
        {
            foreach (string s in Directory.GetDirectories(Path.Combine(Content.WorkingDirectory, "Songs")))
            {
                LoadPack(s, false);
            }
        }

        private static void UpdateCache()
        {
            foreach (string s in Directory.GetDirectories(Path.Combine(Content.WorkingDirectory, "Songs")))
            {
                LoadPack(s, true);
            }
        }

        private static CachedChart LoadCacheFile(string path)
        {
            FileStream fs = new FileStream(path, FileMode.Open);
            TextReader t = new StreamReader(fs);
            var c = new CachedChart { title = t.ReadLine(), creator = t.ReadLine(), artist = t.ReadLine(), abspath = t.ReadLine() };
            t.Close();
            fs.Close();
            return c;
        }

        private static void SaveCacheFile(string path, CachedChart c)
        {
            FileStream fs = new FileStream(path, FileMode.Create);
            TextWriter t = new StreamWriter(fs);
            t.WriteLine(c.title);
            t.WriteLine(c.creator);
            t.WriteLine(c.artist);
            t.WriteLine(c.abspath);
            t.Close();
            fs.Close();
        }

        private static void LoadPack(string folder, bool loadnew)
        {
            CachedChart c;
            string path;
            string packname = Path.GetFileName(folder);
            foreach (string s in Directory.GetDirectories(folder))
            {
                path = Path.Combine(Content.WorkingDirectory,"Data","Cache",packname+Path.GetFileNameWithoutExtension(s)+".cache");
                if (File.Exists(path) && true) //replace true with verification that cache is not out of date
                {
                    c = LoadCacheFile(path);
                }
                else if (loadnew)
                {
                    try
                    {
                        c = CacheChart(LoadFromPath(s,packname));
                        SaveCacheFile(path, c);
                    }
                    catch (Exception e)
                    {
                        //normally when no difficulties are loadable
                        Console.WriteLine("Failed to load chart: "+s+"\n"+e.ToString());
                        continue;
                    }
                }
                else
                {
                    continue;
                }
                c.pack = packname;
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

        private static CachedChart CacheChart(MultiChart c)
        {
            return new CachedChart
            {
                title = c.header.title,
                artist = c.header.artist,
                creator = c.header.creator,
                abspath = c.header.path
            };
        }

        public static MultiChart LoadFromCache(CachedChart c)
        {
            return LoadFromPath(c.abspath, c.pack);
        }
    }
}
