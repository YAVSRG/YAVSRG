using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading;
using YAVSRG.Charts.Stepmania;
using YAVSRG.Charts.YAVSRG;
using YAVSRG.Charts.Osu;
using System.IO;
using System.IO.Compression;
using static YAVSRG.Utilities.Logging;
using static YAVSRG.Utilities.TaskManager;

namespace YAVSRG.Charts
{
    public class ChartLoader
    {
        //this file is a real mess and i need to clean it up

        public static readonly string[] CHARTFORMATS = { ".sm", ".osu", ".yav" };
        public static readonly string[] ARCHIVEFORMATS = { ".osz", ".zip" };

        public static Dictionary<string, Func<CachedChart, string>> GroupBy = new Dictionary<string, Func<CachedChart, string>>()
        {
            { "Physical", (c) => { int i = (int)(c.physical / 2) * 2; return i.ToString().PadLeft(2,'0')+" - "+(i+2).ToString().PadLeft(2,'0'); } },
            { "Technical", (c) => { int i = (int)(c.technical / 2) * 2; return i.ToString().PadLeft(2,'0')+" - "+(i+2).ToString().PadLeft(2,'0'); } },
            { "Creator", (c) => { return Utils.FormatFirstCharacter(c.creator); } },
            { "Artist", (c) => { return Utils.FormatFirstCharacter(c.artist); } },
            { "Pack", (c) =>  { return c.pack; } },
            { "Title", (c) => { return Utils.FormatFirstCharacter(c.title); } },
            { "Keymode",  (c) => { return c.keymode.ToString() + "k"; } },
            { "Collection",  (c) => { return ""; } }
        };

        public static Dictionary<string, Comparison<CachedChart>> SortBy = new Dictionary<string, Comparison<CachedChart>>()
        {
            { "Physical", (a, b) => (a.physical.CompareTo(b.physical)) },
            { "Technical", (a, b) => (a.technical.CompareTo(b.technical)) },
            { "Title", (a, b) => (a.title.CompareTo(b.title)) },
            { "Creator ",(a, b) => (a.creator.CompareTo(b.creator)) },
            { "Artist", (a, b) => (a.artist.CompareTo(b.artist)) },
        };

        public static string SearchString = "";
        public static event Action OnRefreshGroups = () => { };
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
        }

        #region sorting and searching

        private static void SortIntoGroups(Func<CachedChart, string> groupBy, Comparison<CachedChart> sortBy)
        {
            Groups = new List<ChartGroup>();
            Dictionary<string, List<CachedChart>> temp = new Dictionary<string, List<CachedChart>>();
            if (groupBy == GroupBy["Collection"])
            {
                foreach (string c in Game.Gameplay.Collections.Collections.Keys)
                {
                    temp.Add(c, new List<CachedChart>());
                    foreach (string id in Game.Gameplay.Collections.GetCollection(c).Entries)
                    {
                        if (Cache.Charts.ContainsKey(id))
                        {
                            temp[c].Add(Cache.Charts[id]);
                        }
                        else
                        {
                            Log(id + "isn't present in the cache! Maybe it was deleted?", LogType.Warning);
                        }
                    }
                }
            }
            else
            {
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
            }
            foreach (string k in temp.Keys) //why do i do it like this
            {
                ChartGroup g = new ChartGroup(temp[k], k);
                g.Sort(sortBy, false);
                Groups.Add(g);
            }
            Groups.Sort((a, b) => { return a.label.CompareTo(b.label); });
        }

        private static void SearchGroups(string s)
        {
            s = s.ToLower();
            SearchResult = new List<ChartGroup>();
            bool keymodeMatch;
            string summaryData;
            foreach (ChartGroup g in Groups)
            {
                List<CachedChart> temp = new List<CachedChart>();
                foreach (CachedChart c in g.charts)
                {
                    keymodeMatch = Game.Options.Profile.Keymode == 0 || c.keymode == Game.Options.Profile.Keymode;
                    summaryData = (c.title + " " + c.creator + " " + c.artist + " " + c.diffname + " " + c.pack).ToLower();
                    if (keymodeMatch && summaryData.Contains(s))
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
            if (!(GroupBy.ContainsKey(Game.Options.Profile.ChartGroupMode) && (SortBy.ContainsKey(Game.Options.Profile.ChartSortMode))))
            {
                Log("Invalid sort or search mode", LogType.Warning);
                return;
            }
            SortIntoGroups(GroupBy[Game.Options.Profile.ChartGroupMode], SortBy[Game.Options.Profile.ChartSortMode]);
            SearchGroups(SearchString);
            OnRefreshGroups();
        }

        public static void RefreshCallback(bool b)
        {
            if (b) Refresh();
        }

        #endregion

        public static void RandomChart()
        {
            if (Cache.Charts.Count == 0)
            {
                Chart def = new Chart(new List<Snap>(), new ChartHeader { SourcePack = "Nowhere", Artist = "Percyqaz", Creator = "Nobody", Title = "You have no songs installed!", SourcePath = Game.WorkingDirectory, DiffName = "Default", AudioFile = "", BGFile = "", PreviewTime = 0, File = "" }, 4);
                def.Timing.SetTimingData(new List<BPMPoint>());
                Game.Gameplay.ChangeChart(null, def, false);
                return;
            }
            SwitchToChart(Cache.Charts.Values.ToList()[new Random().Next(0, Cache.Charts.Values.Count)], true);
        }
        
        public static UserTask Recache()
        {
            return (Output) =>
            {
                lock (Cache)
                {
                    Cache.Charts = new Dictionary<string, CachedChart>(); //clear cache
                    foreach (string pack in Directory.EnumerateDirectories(Path.Combine(Game.WorkingDirectory, "Songs")))
                    {
                        foreach (string song in Directory.EnumerateDirectories(pack))
                        {
                            foreach (string f in Directory.EnumerateFiles(song))
                            {
                                if (Path.GetExtension(f).ToLower() == ".yav")
                                {
                                    try
                                    {
                                        Output("Caching: " + f);
                                        Cache.CacheChart(Chart.FromFile(f));
                                    }
                                    catch (Exception e)
                                    {
                                        Log("Could not cache chart: " + f + "\n" + e.Message + "\n" + e.StackTrace, LogType.Error);
                                    }
                                }
                            }
                        }
                    }
                    Cache.Save();
                    Output("Saved cache.");
                }
                return true;
            };
        }

        #region deleting

        //todo: fix this (it's not complete and will delete extra charts in the same folder by mistake and stuff)
        public static void DeleteChart(CachedChart c)
        {
            lock (Cache)
            {
                try
                {
                    Cache.Charts.Remove(c.GetFileIdentifier());
                    Directory.Delete(Path.GetDirectoryName(c.GetFileIdentifier()), true);
                }
                catch
                {
                    //folders will be "deleted" twice for multi difficulties so errors are expected
                }
            }
        }

        //todo: fix this also
        public static void DeleteGroup(ChartGroup group)
        {
            foreach (CachedChart c in group.charts)
            {
                DeleteChart(c);
            }
            Cache.Save();
        }

        #endregion

        #region conversions

        public static UserTask ConvertAllPacks(string path, bool trackChildren)
        {
            return (Output) =>
            {
                foreach (string s in Directory.EnumerateDirectories(path))
                {
                    Output("Converting: " + Path.GetFileName(s));
                    Game.Tasks.AddTask(ConvertPack(s, Path.GetFileName(s), false), (b) => { }, "Convert pack: " + Path.GetFileName(s), trackChildren);
                }
                Cache.Save();
                return true;
            };
        }

        public static UserTask ConvertPack(string PackFolder, string PackName, bool SaveAfter) //name not derived from the folder so i can name the osu songs folder something other than "Songs".
        {
            return (Output) =>
            {
                foreach (string SongFolder in Directory.EnumerateDirectories(PackFolder))
                {
                    Output("Converting files in: " + Path.GetFileName(SongFolder));
                    ConvertSongFolder(SongFolder, PackName)(Output);
                }
                if (SaveAfter) Cache.Save();
                return true;
            };
        }

        public static UserTask ConvertSongFolder(string SongFolder, string PackName)
        {
            return (Output) =>
            {
                foreach (string File in Directory.EnumerateFiles(SongFolder))
                {
                    if (CHARTFORMATS.Contains(Path.GetExtension(File).ToLower()))
                    {
                        ConvertFile(File, PackName); //errors are handled inside here
                    }
                }
                return true;
            };
        }

        private static void ConvertFile(string AbsFilepath, string TargetPack) //pack only used if importing from an external pack
        {
            string SongFolder = Path.GetDirectoryName(AbsFilepath);
            string TargetSongFolder = Path.Combine(Game.WorkingDirectory, "Songs", TargetPack, Path.GetFileName(SongFolder));
            string ext = Path.GetExtension(AbsFilepath).ToLower();

            if (ext == ".osu")
            {
                try
                {
                    OsuBeatmap Beatmap = new OsuBeatmap(Path.GetFileName(AbsFilepath), Path.GetDirectoryName(AbsFilepath));
                    if (Beatmap.Mode == 3) { Chart Converted = Beatmap.Convert(); ConvertToInterlude(Converted, SongFolder, TargetSongFolder); Log("Converted " + AbsFilepath); }
                }
                catch (Exception e)
                {
                    Log("Could not convert .osu file: " + AbsFilepath + "\n    " + e.ToString(), LogType.Error);
                }
            }
            else if (ext == ".sm")
            {
                try
                {
                    StepFile Stepfile = new StepFile(Path.GetFileName(AbsFilepath), Path.GetDirectoryName(AbsFilepath));
                    int i = 0;
                    foreach (Chart Difficulty in Stepfile.Convert())
                    {
                        if (i > 0)
                        {
                            Difficulty.Data.File = Difficulty.Data.File.Replace(".sm", "_" + i.ToString() + ".sm"); //file extension is edited later
                        }
                        ConvertToInterlude(Difficulty, SongFolder, TargetSongFolder);
                        i++;
                    }
                    Log("Converted " + AbsFilepath + " (" + i.ToString() + ")");
                }
                catch (Exception e)
                {
                    Log("Could not convert .sm file: " + AbsFilepath + "\n    " + e.ToString(), LogType.Error);
                }
            }
            else if (ext == ".yav")
            {
                try
                {
                    Chart Chart = Chart.FromFile(AbsFilepath);
                    ConvertToInterlude(Chart, SongFolder, TargetSongFolder);
                    Log("Converted " + AbsFilepath);
                }
                catch (Exception e)
                {
                    Log("Could not \"convert\" .yav file: " + AbsFilepath + "\n    " + e.ToString(), LogType.Error);
                }
            }
        }

        private static void ConvertToInterlude(Chart chart, string sourceFolder, string targetFolder) //todo: swap names of ConvertFile and ConvertChart
        {
            chart.Data.SourcePack = Path.GetFileName(Path.GetDirectoryName(targetFolder));
            chart.Data.SourcePath = targetFolder;
            chart.Data.File = Path.ChangeExtension(chart.Data.File, ".yav");
            Directory.CreateDirectory(targetFolder); //only creates if needs to
            //this will copy the externally sourced chart to an appropriate folder in the songs folder
            CopyFile(Path.Combine(sourceFolder, chart.Data.AudioFile), Path.Combine(targetFolder, chart.Data.AudioFile));
            CopyFile(Path.Combine(sourceFolder, chart.Data.BGFile), Path.Combine(targetFolder, chart.Data.BGFile));
            chart.WriteToFile(Path.Combine(targetFolder, chart.Data.File));
            lock (Cache)
            {
                Cache.CacheChart(chart);
            }
        }

        private static void CopyFile(string source, string target)
        {
            if (File.Exists(source))
            {
                if (!File.Exists(target))
                {
                    try
                    {
                        File.Copy(source, target);
                    }
                    catch (Exception e)
                    {
                        Log("Couldn't copy media file from " + source + ": " + e.ToString(), LogType.Error);
                    }
                }
            }
            else
            {
                Log("Missing media file for a chart: " + source, LogType.Warning);
            }
        }

        #endregion

        #region imports

        public static UserTask DownloadAndImportPack(string Url, string PackTitle)
        {
            string DownloadPath = Path.Combine(Game.WorkingDirectory, "Imports", PackTitle + ".zip");
            return (Output) =>
            {
                Output("Downloading from " + Url + " ...");
                if (Net.Web.WebUtils.DownloadFile(Url, DownloadPath)(Output))
                {
                    Output("Downloaded!");
                    Game.Tasks.AddTask(ImportArchive(DownloadPath), RefreshCallback, "Import: " + PackTitle, true);
                    return true;
                }
                else
                {
                    Output("An error occured while downloading!");
                    return false;
                }
            };
        }
        
        public static UserTask ImportOsu()
        {
            return (Output) =>
            {
                    string Folder = GetOsuSongFolder();
                    if (Directory.Exists(Folder))
                    {
                        Output("Detected osu! Folder");
                        Game.Tasks.AddTask(ConvertPack(Folder, "osu! Imports", true), RefreshCallback, "Importing osu! songs", true);
                        Output("Converted osu! songs folder successfully.");
                        return true;
                    }
                    else
                    {
                        Output("Could not detect osu! folder! You'll have to drop it here if it's installed in a custom location.");
                        return false;
                    }
            };
        }

        public static UserTask ImportStepmania()
        {
            return (Output) =>
            {
                    string Folder = Path.Combine(Path.GetPathRoot(Game.WorkingDirectory), "Games", "Etterna", "Songs");
                    if (Directory.Exists(Folder))
                    {
                        Output("Detected Etterna!");
                    }
                    else
                    {
                        Folder = Path.Combine(Path.GetPathRoot(Game.WorkingDirectory), "Games", "Stepmania 5", "Songs");
                        if (Directory.Exists(Folder))
                        {
                            Output("Detected Stepmania 5!");
                        }
                        else
                        {
                            Output("Could not detect Stepmania/Etterna song folders. You'll have to drop it here if in a custom location.");
                            return false;
                        }
                    }
                    Game.Tasks.AddTask(ConvertAllPacks(Folder, true), RefreshCallback, "Importing Stepmania charts", false);
                    return true;
            };
        }
                
        //todo: fully bug proof this as still not stable
        private static UserTask ImportArchive(string path)
        {
            return (Output) =>
            {
                if (Path.GetExtension(path).ToLower() == ".osz") //ASSUMES IT IS A CORRECTLY FORMATTED .OSZ AND NOT MALICIOUSLY STRUCTURED. TAKE CARE OF YOURSELF
                {
                    string dir = Path.Combine(Game.WorkingDirectory, "Imports", "osu! Imports", Path.GetFileNameWithoutExtension(path));
                    using (ZipArchive z = ZipFile.Open(path, ZipArchiveMode.Read))
                    {
                        z.ExtractToDirectory(dir);
                    }
                    Game.Tasks.AddTask(ConvertSongFolder(dir, "osu! Imports"), (b) =>
                    {
                        RefreshCallback(b);
                        try
                        {
                            File.Delete(path);
                            Directory.Delete(dir, true);
                        }
                        catch (Exception e)
                        {
                            Log("Couldn't delete files after extracting .osz: " + e.ToString(), LogType.Warning);
                        }

                    }, "Import .osz files", false);
                    return true;
                }
                else if (Path.GetExtension(path).ToLower() == ".zip")
                {
                    string root = "";
                    bool valid = false;
                    string ext;
                    using (ZipArchive z = ZipFile.Open(path, ZipArchiveMode.Read))
                    {
                        foreach (ZipArchiveEntry e in z.Entries)
                        {
                            ext = Path.GetExtension(e.Name).ToLower();
                            if (ext == ".sm" || ext == ".osu" || ext == ".yav")
                            {
                                root = Path.GetDirectoryName(Path.GetDirectoryName(e.FullName));
                                valid = true;
                                break;
                            }
                        }
                        string target = Path.Combine(Game.WorkingDirectory, "Imports", Path.GetFileNameWithoutExtension(path));
                        if (!valid || root == "") //todo: fix flat folders as zips
                        {
                            Output("Couldn't find anything to extract in this archive.");
                            return false;
                        }
                        foreach (ZipArchiveEntry e in z.Entries)
                        {
                            if (e.FullName.StartsWith(root, StringComparison.Ordinal))
                            {
                                string f = Path.Combine(target + e.FullName.Substring(root.Length));
                                if (Path.GetExtension(f) != "")
                                {
                                    try
                                    {
                                        e.ExtractToFile(Path.Combine(target + e.FullName.Substring(root.Length)));
                                    }
                                    catch
                                    {
                                        //probably already exists, don't worry about it
                                    }
                                }
                                else
                                {
                                    Directory.CreateDirectory(f);
                                }
                            }
                        }
                        Game.Tasks.AddTask(ConvertPack(target, Path.GetFileNameWithoutExtension(path), true), (b) =>
                        {
                            RefreshCallback(b);
                            try
                            {
                                File.Delete(path);
                                Directory.Delete(target, true);
                            }
                            catch (Exception e)
                            {
                                Log("Couldn't delete files after extracting .zip: " + e.ToString(), LogType.Warning);
                            }

                        }, "Importing pack: " + Path.GetFileNameWithoutExtension(path), true);
                    }
                    return true;
                }
                else
                {
                    Output("Unsupported file format.");
                    return false;
                }
                //no rar support until i know how
            };
        }

        //todo: support for single song folders and also rar hopefully
        public static UserTask AutoImportFromPath(string path)
        {
            return (Output) =>
            {
                string ext = Path.GetExtension(path).ToLower();
                if (Directory.Exists(path))
                {
                    foreach (string folder in Directory.EnumerateDirectories(path))
                    {
                        foreach (string entry in Directory.EnumerateFileSystemEntries(folder))
                        {
                            ext = Path.GetExtension(entry).ToLower();
                            if (CHARTFORMATS.Contains(ext))
                            {
                                //we've found a pack: folder of song folders
                                Output("Detected pack!");
                                Game.Tasks.AddTask(ConvertPack(path, Path.GetFileName(path), true), RefreshCallback, "Import pack: " + Path.GetFileName(path), true);
                                Output("Imported pack successfully.");
                                return true;
                            }
                            else if (Directory.Exists(entry))
                            {
                                foreach (string file in Directory.EnumerateFiles(entry))
                                {
                                    ext = Path.GetExtension(file).ToLower();
                                    if (CHARTFORMATS.Contains(ext))
                                    {
                                        //we've found a songs folder: folder of packs
                                        Output("Detected folder of packs!");
                                        Game.Tasks.AddTask(ConvertAllPacks(path, true), RefreshCallback, "Import songs folder: "+Path.GetFileName(path), true);
                                        Output("Imported pack folder successfully.");
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                    //if you're here this could be a song folder: folder with audio file and chart file
                    //it could also be just a random folder some idiot dragged in
                    //foreach (string file in Directory.EnumerateFiles(path))
                    //{
                    //}
                    Output("Found nothing to import in this folder.");
                    return false;
                }
                else if (ARCHIVEFORMATS.Contains(ext))
                {
                    Output("Detected archive!");
                    Game.Tasks.AddTask(ImportArchive(path), (b) => { }, "Import archive: " + Path.GetFileNameWithoutExtension(path), true);
                    return true;
                }
                else
                {
                    Output("This cannot be converted!");
                    return false;
                }
            };
        }

        public static string GetOsuSongFolder()
        {
            return Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "osu!", "Songs");
        }

        #endregion imports

        public static void SwitchToChart(CachedChart c, bool playFromPreview)
        {
            Chart chart = Cache.LoadChart(c);
            if (chart != null)
            {
                Game.Gameplay.ChangeChart(c, chart, playFromPreview);
            }
            else
            {
                Log("Can't switch to chart because it can't be found!", LogType.Error);
            }
        }
    }
}
