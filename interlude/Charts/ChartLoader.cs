using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using YAVSRG.Charts.Stepmania;
using YAVSRG.Charts.YAVSRG;
using YAVSRG.Charts.Osu;
using System.IO;

namespace YAVSRG.Charts
{
    public class ChartLoader
    {
        public static Func<CachedChart, string> GroupByPack = (c) => { return c.pack; };
        public static Func<CachedChart, string> GroupByTitle = (c) => { return Utils.FormatFirstCharacter(c.title); };
        public static Func<CachedChart, string> GroupByDifficulty = (c) => { int i = (int)(c.physical / 2) * 2; return i.ToString()+" - "+(i+2).ToString(); };
        public static Func<CachedChart, string> GroupByCreator = (c) => { return Utils.FormatFirstCharacter(c.creator); };
        public static Func<CachedChart, string> GroupByArtist = (c) => { return Utils.FormatFirstCharacter(c.artist); };

        public static Comparison<CachedChart> SortByDifficulty = (a, b) => (a.physical.CompareTo(b.physical));
        public static Comparison<CachedChart> SortByTitle = (a, b) => (a.title.CompareTo(b.title));
        public static Comparison<CachedChart> SortByCreator = (a, b) => (a.creator.CompareTo(b.creator));
        public static Comparison<CachedChart> SortByArtist = (a, b) => (a.artist.CompareTo(b.artist));

        public static Comparison<CachedChart> SortMode = SortByTitle;
        public static Func<CachedChart, string> GroupMode = GroupByPack;
        public static string SearchString = "";
        public static event Action OnRefreshGroups = () => { };
        public static bool Loaded;
        public static List<ChartGroup> Groups;
        public static List<ChartGroup> SearchResult;
        public static Cache Cache;

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

        public static void Init()
        {
            Groups = new List<ChartGroup>();
            Cache = Cache.LoadCache();
            //ConvertPack(GetOsuSongFolder(), "osu! Imports");
            //Cache.Save();
            Loaded = true;
        }

        public static void SortIntoGroups(Func<CachedChart, string> groupBy, Comparison<CachedChart> sortBy)
        {
            Groups = new List<ChartGroup>();
            Dictionary<string, List<CachedChart>> temp = new Dictionary<string, List<CachedChart>>();
            string s;
            foreach (CachedChart c in Cache.Charts.Values)
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
                    keymodeMatch = Game.Options.Profile.Keymode == 0 || c.keymode == Game.Options.Profile.Keymode;
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
            if (Cache.Charts.Count == 0)
            {
                Game.Gameplay.ChangeChart(new Chart(new List<Snap>(), new List<BPMPoint>(), new ChartHeader { SourcePack = "Nowhere", Artist = "Percyqaz", Creator = "Nobody", Title = "You have no songs installed!", SourcePath = Content.WorkingDirectory, DiffName = "Default", AudioFile = "", BGFile = "", PreviewTime = 0 }, 4));
                return;
            }
            SwitchToChart(Cache.Charts.Values.ToList()[new Random().Next(0, Cache.Charts.Values.Count)]);
        }
        
        public static void Recache()
        {
            foreach (string s in Directory.GetDirectories(Path.Combine(Content.WorkingDirectory, "Songs")))
            {
                foreach (string f in Directory.GetFiles(s))
                {
                    if (Path.GetExtension(f) == ".yav")
                    {
                        Cache.CacheChart(Chart.FromFile(f));
                    }
                }
            }
            SaveCache();
        }

        public static void RecacheThreaded()
        {
            //Loaded = false;
            //Thread t = new Thread(new ThreadStart(() => { UpdateCache(); Loaded = true; }));
            //t.Start();
        }

        private static void SaveCache()
        {
            string s = Path.Combine(Content.WorkingDirectory, "Data", "Cache.json");
            Utils.SaveObject(Cache, s);
        }

        public static void ConvertPack(string path, string name) //name not derived from the folder so i can name the osu songs folder something other than "Songs".
        {
            foreach (string songfolder in Directory.GetDirectories(path))
            {
                foreach (string file in Directory.GetFiles(songfolder))
                {
                    try
                    {
                        ConvertChart(file, name); //if it's not a chart file it'll get ignored
                    }
                    catch
                    {

                    }
                }
            }
        }

        public static void ConvertChart(string absfilepath, string pack) //pack only used if importing from an external pack
        {
            Chart c = null;

            if (Path.GetExtension(absfilepath) == ".osu")
            {
                try
                {
                    var o = new OsuBeatmap(Path.GetFileName(absfilepath), Path.GetDirectoryName(absfilepath));
                    if (o.Mode == 3) { c = o.Convert(); }
                }
                catch { }
            }
            else if (Path.GetExtension(absfilepath) == ".sm")
            {
                //some stuff
            }

            if (c != null)
            {
                c.Data.SourcePack = pack;
                if (Path.GetDirectoryName(Path.GetDirectoryName(Path.GetDirectoryName(absfilepath))) != Path.Combine(Content.WorkingDirectory, "Songs"))
                {
                    string sourceFolder = Path.GetDirectoryName(absfilepath);
                    string targetFolder = Path.Combine(Content.WorkingDirectory,"Songs",pack,Path.GetFileName(sourceFolder));
                    c.Data.SourcePath = targetFolder;
                    c.Data.File = Path.ChangeExtension(c.Data.File, ".yav");
                    Directory.CreateDirectory(targetFolder);
                    //this will copy the externally sourced chart to an appropriate folder in the songs folder
                    try
                    {
                        File.Copy(Path.Combine(sourceFolder, c.Data.AudioFile), Path.Combine(targetFolder, c.Data.AudioFile));
                    }
                    catch { }
                    try
                    {
                        File.Copy(Path.Combine(sourceFolder, c.Data.BGFile), Path.Combine(targetFolder, c.Data.BGFile));
                    }
                    catch { }
                    c.WriteToFile(Path.Combine(targetFolder, Path.GetFileNameWithoutExtension(absfilepath) + ".yav"));
                }
                else
                {
                    //this will create a new file here (in the songs folder) and rename the old file to .old
                    c.WriteToFile(Path.ChangeExtension(absfilepath, ".yav"));
                    File.Move(absfilepath, absfilepath + ".old");
                }
                Cache.CacheChart(c);
            }
        }

        public static string GetOsuSongFolder()
        {
            return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "osu!", "Songs");
        }

        public static void SwitchToChart(CachedChart c)
        {
            Game.Gameplay.ChangeChart(Cache.LoadChart(c));
        }
    }
}
