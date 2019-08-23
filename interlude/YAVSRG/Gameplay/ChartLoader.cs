using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.IO.Compression;
using System.Drawing;
using Prelude.Gameplay.Charts.Stepmania;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay.Charts.Osu;
using Prelude.Gameplay.DifficultyRating;
using static Prelude.Utilities.Logging;
using static Interlude.Utilities.TaskManager;

namespace Interlude.Gameplay
{
    public class ChartLoader
    {
        //A lot of operations have been reformatted as UserTasks. To run them, use Game.Tasks.AddTask(<Method>,<Callback to run when it completes>,<Name to display for it>,<Should it be displayed in task list>)

        //Constant lists indicating what is supported for conversion. Extend them here if support for a new archive or file format is implemented
        public static readonly string[] CHARTFORMATS = { ".sm", ".osu", ".yav" };
        public static readonly string[] ARCHIVEFORMATS = { ".osz", ".zip" };
        public static readonly string OSU_PACK_TITLE = "osu!";

        //All of the grouping methods for charts
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

        //All of the sorting methods for charts
        public static Dictionary<string, Comparison<CachedChart>> SortBy = new Dictionary<string, Comparison<CachedChart>>()
        {
            { "Physical", (a, b) => a.physical.CompareTo(b.physical) },
            { "Technical", (a, b) => a.technical.CompareTo(b.technical) },
            { "Title", (a, b) => a.title.CompareTo(b.title) },
            { "Creator ",(a, b) => a.creator.CompareTo(b.creator) },
            { "Artist", (a, b) => a.artist.CompareTo(b.artist) },
        };

        public static Dictionary<string, Func<CachedChart, Color>> ColorBy = new Dictionary<string, Func<CachedChart, Color>>()
        {
            { "Nothing", (c) => { return Game.Options.Theme.SelectChart; } },
            { "Physical", (c) => { return CalcUtils.PhysicalColor(c.physical); } },
            { "Technical", (c) => { return CalcUtils.TechnicalColor(c.technical); } },
            { "BPM", (c) => { return CalcUtils.TechnicalColor(c.bpm*0.05f); } },
            { "Length", (c) => { return CalcUtils.TechnicalColor(c.length*0.000033f); } },
        };

        public static string SearchString = "";
        public static event Action OnRefreshGroups = () => { };
        public static List<ChartGroup> GroupedCharts;
        public static Cache Cache;

        //Structure to hold grouped charts (for level selection menu)
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

        //Loads charts/cache from file. Called when game starts
        public static void Init()
        {
            GroupedCharts = new List<ChartGroup>();
            Cache = Cache.LoadCache();
        }

        #region sorting and searching

        //Groups all cached charts by the selected criteria - Used by level select screen
        private static List<ChartGroup> SortIntoGroups(Func<CachedChart, string> groupBy, Comparison<CachedChart> sortBy)
        {
            List<ChartGroup> Groups = new List<ChartGroup>();
            Dictionary<string, List<CachedChart>> temp = new Dictionary<string, List<CachedChart>>(); //holds the temp data as groups are being put together
            List<string> toRemove;
            if (groupBy == GroupBy["Collection"]) //Collections have different behaviour (can't look up collection from chart, only reverse)
            {
                foreach (string c in Game.Gameplay.Collections.Collections.Keys)
                {
                    temp.Add(c, new List<CachedChart>());
                    toRemove = new List<string>();
                    lock (Cache)
                    {
                        foreach (string id in Game.Gameplay.Collections.GetCollection(c).Entries)
                        {
                            if (Cache.Charts.ContainsKey(id))
                            {
                                temp[c].Add(Cache.Charts[id]);
                            }
                            else
                            {
                                //todo: alert the user of this and/or remove chart from collection
                                //toRemove.Add(id);
                                Log(id + "isn't present in the cache! Maybe it was deleted?", "", LogType.Warning);
                            }
                        }
                    }
                    foreach (string id in toRemove)
                    {
                        Game.Gameplay.Collections.GetCollection(c).Entries.Remove(id);
                    }
                }
            }
            else //Grouping logic
            {
                string s;
                lock (Cache)
                {
                    foreach (CachedChart c in Cache.Charts.Values)
                    {
                        s = groupBy(c); //Gets group (as a string) to put this chart in
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
            }
            foreach (string k in temp.Keys) //Sort completed groups and make them accessible by level select
            {
                ChartGroup g = new ChartGroup(temp[k], k);
                g.Sort(sortBy, false);
                Groups.Add(g);
            }
            Groups.Sort((a, b) => { return a.label.CompareTo(b.label); }); //Sort overall list of groups alphabetically
            return Groups;
        }

        //Filters all groups by search criteria - Used by level select screen
        private static List<ChartGroup> SearchGroups(string Criteria, List<ChartGroup> Groups)
        {
            Criteria = Criteria.ToLower();
            List<ChartGroup> Result = new List<ChartGroup>();
            bool keymodeMatch;
            string summaryData;
            int keymode = (int)Game.Options.Profile.PreferredKeymode + 3;
            foreach (ChartGroup g in Groups)
            {
                List<CachedChart> temp = new List<CachedChart>();
                foreach (CachedChart c in g.charts)
                {
                    //todo: rewrite match algorithm to allow for more detailed filters
                    keymodeMatch = !Game.Options.Profile.KeymodePreference || c.keymode == keymode;
                    summaryData = (c.title + " " + c.creator + " " + c.artist + " " + c.diffname + " " + c.pack).ToLower();
                    if (keymodeMatch && summaryData.Contains(Criteria))
                    {
                        temp.Add(c);
                    }
                }
                if (temp.Count > 0)
                {
                    //Finalised groups as shown on level select - Including number of charts in this group
                    Result.Add(new ChartGroup(temp, g.label + " (" + temp.Count.ToString() + ")"));
                }
            }
            return Result;
        }

        //Re-sorts/searches charts for use in level select screen.
        public static void Refresh()
        {
            if (!(GroupBy.ContainsKey(Game.Options.Profile.ChartGroupMode) && (SortBy.ContainsKey(Game.Options.Profile.ChartSortMode))))
            {
                Log("Invalid sort or search mode. Use the level select screen to change them.", "", LogType.Warning);
                return;
            }
            
            GroupedCharts = SearchGroups(SearchString, SortIntoGroups(GroupBy[Game.Options.Profile.ChartGroupMode], SortBy[Game.Options.Profile.ChartSortMode]));
            OnRefreshGroups();
        }

        //Shorthand for when usertasks complete that require the groups to be refreshed as charts are now added
        public static void RefreshCallback(bool b)
        {
            if (b) Refresh();
        }

        #endregion

        public static void SelectDefaultChart()
        {
            if (Cache.Charts.Count == 0)
            {
                Game.Screens.ChangeBackground(Game.Options.Themes.GetTexture("background"));
                Game.Screens.ChangeThemeColor(Game.Options.Theme.DefaultThemeColor);
                return;
            }
            if (Cache.Charts.ContainsKey(Game.Options.General.LastSelectedFile))
                SwitchToChart(Cache.Charts[Game.Options.General.LastSelectedFile], true);
            if (Game.CurrentChart == null) SwitchToChart(Cache.Charts.Values.ToList()[new Random().Next(0, Cache.Charts.Values.Count)], true);
        }

        //Task to recache all charts (useful if you deleted them manually but they're still cached, or the cache is broken and charts are missing from it)
        //It's essentially a repair tool and you shouldn't need to use it normally
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
                                        Log("Could not cache chart from " + f, e.ToString(), LogType.Error);
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

        //Converts all packs in a given songs folder (i.e Songs folder for Stepmania)
        //It just creates a pack conversion task for every pack in the folder.
        //TrackChildren flags if the subtasks should be visible to the user
        public static UserTask ConvertAllPacks(string Path, bool TrackChildren)
        {
            return (Output) =>
            {
                foreach (string s in Directory.EnumerateDirectories(Path))
                {
                    Output("Converting: " + System.IO.Path.GetFileName(s));
                    Game.Tasks.AddTask(ConvertPack(s, System.IO.Path.GetFileName(s)), (b) => { }, "Convert pack: " + System.IO.Path.GetFileName(s), TrackChildren);
                }
                return true;
            };
        }

        //Converts all charts in a pack to interlude (and moves them to the right place)
        //PackName is the name of the new pack they are moved to
        public static UserTask ConvertPack(string PackFolder, string PackName)
        {
            return (Output) =>
            {
                foreach (string SongFolder in Directory.EnumerateDirectories(PackFolder))
                {
                    Output("Converting files in: " + Path.GetFileName(SongFolder));
                    ConvertSongFolder(SongFolder, PackName)(Output);
                }
                Cache.Save();
                return true;
            };
        }

        //Converts a song folder (contains mp3, bg, chart file) to .yav and copies it to correct place
        //Cache is not saved after this operation (to reduce amount of times saved) so if you run this as a one off, remember to save the cache!
        public static UserTask ConvertSongFolder(string SongFolder, string PackName)
        {
            return (Output) =>
            {
                foreach (string File in Directory.EnumerateFiles(SongFolder))
                {
                    if (CHARTFORMATS.Contains(Path.GetExtension(File).ToLower()))
                    {
                        ConvertFile(File, PackName); //errors are handled inside here so no catching needed
                    }
                }
                return true;
            };
        }

        //Converts a single file (.sm, .osu, .yav etc) from its exact path to .yav in the correct location.
        //This just inteprets Chart instances from the file and then runs ConvertToInterlude on them.
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
                    if (Beatmap.Mode == 3) { Chart Converted = Beatmap.Convert(); ConvertToInterlude(Converted, SongFolder, TargetSongFolder); Log("Converted " + AbsFilepath, "", LogType.Debug); }
                }
                catch (Exception e)
                {
                    Log("Could not convert .osu file " + AbsFilepath, e.ToString(), LogType.Error);
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
                    Log("Converted " + AbsFilepath + " (" + i.ToString() + ")", "", LogType.Debug);
                }
                catch (Exception e)
                {
                    Log("Could not convert .sm file " + AbsFilepath, e.ToString(), LogType.Error);
                }
            }
            else if (ext == ".yav")
            {
                try
                {
                    Chart Chart = Chart.FromFile(AbsFilepath);
                    ConvertToInterlude(Chart, SongFolder, TargetSongFolder);
                    Log("Converted " + AbsFilepath, "", LogType.Debug);
                }
                catch (Exception e)
                {
                    Log("Could not \"convert\" .yav file " + AbsFilepath, e.ToString(), LogType.Error);
                }
            }
        }

        //Converts a Chart (the note data read from a file) by saving it as .yav in the correct place and copying BG/audio files if needed
        private static void ConvertToInterlude(Chart Chart, string SourceFolder, string TargetFolder)
        {
            //rewrite metadata
            Chart.Data.SourcePack = Path.GetFileName(Path.GetDirectoryName(TargetFolder));
            Chart.Data.SourcePath = TargetFolder;
            Chart.Data.File = Path.ChangeExtension(Chart.Data.File, ".yav");

            Directory.CreateDirectory(TargetFolder); //only creates if needs to

            //this will copy the externally sourced chart to an appropriate folder in the songs folder
            CopyFile(Path.Combine(SourceFolder, Chart.Data.AudioFile), Path.Combine(TargetFolder, Chart.Data.AudioFile));
            CopyFile(Path.Combine(SourceFolder, Chart.Data.BGFile), Path.Combine(TargetFolder, Chart.Data.BGFile));
            Chart.WriteToFile(Path.Combine(TargetFolder, Chart.Data.File));

            //add chart to cache
            Cache.CacheChart(Chart);
        }

        //Copies a media file (bg/audio) from a source to a target location
        private static void CopyFile(string SourcePath, string TargetPath)
        {
            if (File.Exists(SourcePath))
            {
                if (!File.Exists(TargetPath))
                {
                    try
                    {
                        File.Copy(SourcePath, TargetPath);
                    }
                    catch (Exception e)
                    {
                        Log("Couldn't copy media file from " + SourcePath, e.ToString(), LogType.Error);
                    }
                }
            }
            else
            {
                Log("Missing media file for a chart ", SourcePath, LogType.Warning);
            }
        }

        #endregion

        #region imports

        public static UserTask DownloadAndImportPack(string Url, string PackTitle, string FileExtension)
        {
            string DownloadPath = Path.Combine(Game.WorkingDirectory, "Imports", PackTitle + FileExtension);
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
                        Game.Tasks.AddTask(ConvertPack(Folder, OSU_PACK_TITLE), RefreshCallback, "Importing osu! songs", true);
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
                    Game.Tasks.AddTask(ConvertSongFolder(dir, OSU_PACK_TITLE), (b) =>
                    {
                        RefreshCallback(b);
                        try
                        {
                            File.Delete(path);
                            Directory.Delete(dir, true);
                        }
                        catch (Exception e)
                        {
                            Log("Couldn't delete files after extracting .osz", e.ToString(), LogType.Warning);
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
                        Game.Tasks.AddTask(ConvertPack(target, Path.GetFileNameWithoutExtension(path)), (b) =>
                        {
                            RefreshCallback(b);
                            try
                            {
                                File.Delete(path);
                                Directory.Delete(target, true);
                            }
                            catch (Exception e)
                            {
                                Log("Couldn't delete files after extracting .zip", e.ToString(), LogType.Warning);
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
                                Game.Tasks.AddTask(ConvertPack(path, Path.GetFileName(path)), RefreshCallback, "Import pack: " + Path.GetFileName(path), true);
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
                Game.Options.General.LastSelectedFile = c.GetFileIdentifier();
                Game.Gameplay.ChangeChart(c, chart, playFromPreview);
            }
            else
            {
                Log("Can't switch to chart because it can't be found!", "", LogType.Error);
            }
        }
    }
}
