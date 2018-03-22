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
        public struct ChartPack
        {
            public string title;
            public string desc;
            public CachedChart[] charts;

            public ChartPack(string title, CachedChart[] charts)
            {
                this.title = title;
                desc = "";
                this.charts = charts;
            }
        }

        public struct CachedChart
        {
            public string title;
            public string artist;
            public string creator;
            public string abspath;
            public float[] difficulty;

            public CachedChart(string a, string b, string c, string d)
            {
                difficulty = new float[8];
                title = a;
                artist = b;
                creator = c;
                abspath = d;
            }
        }

        public static bool Loaded;
        public static List<ChartPack> Cache;
        public static ChartPack SelectedPack;
        public static MultiChart SelectedChart;

        public static void Init()
        {
            Cache = new List<ChartPack>();
            UpdateCache();
            Loaded = true;
        }

        public static void RandomPack()
        {
            SelectedPack = Cache[new Random().Next(0, Cache.Count)];
            RandomChart();
        }

        public static void RandomChart()
        {
            SelectedChart = LoadFromCache(SelectedPack.charts[new Random().Next(0, SelectedPack.charts.Length)]);
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
            var c = new CachedChart(t.ReadLine(), t.ReadLine(), t.ReadLine(), t.ReadLine());
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
            List<CachedChart> diffs = new List<CachedChart>();
            CachedChart c;
            string path;
            foreach (string s in Directory.GetDirectories(folder))
            {
                path = Path.Combine(Content.WorkingDirectory,"Data","Cache",Path.GetFileName(folder)+Path.GetFileNameWithoutExtension(s)+".cache");
                if (File.Exists(path) && true) //replace true with verification that cache is not out of date
                {
                    c = LoadCacheFile(path);
                }
                else if (loadnew)
                {
                    try
                    {
                        c = CacheChart(LoadFromPath(s));
                        SaveCacheFile(path, c);
                    }
                    catch (Exception e)
                    {
                        Console.WriteLine("Failed to load chart: "+s+"\n"+e.ToString());
                        continue;
                    }
                }
                else
                {
                    continue;
                }
                diffs.Add(c);
            }
            ChartPack p = new ChartPack(Path.GetFileName(folder),diffs.ToArray());
            Cache.Add(p);
        }

        public static MultiChart LoadFromPath(string p)
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
            return c;
        }

        private static CachedChart CacheChart(MultiChart c)
        {
            return new CachedChart(
                    c.header.title, c.header.artist, c.header.creator, c.header.path
                );
        }

        public static MultiChart LoadFromCache(CachedChart c)
        {
            return LoadFromPath(c.abspath);
        }
    }
}
