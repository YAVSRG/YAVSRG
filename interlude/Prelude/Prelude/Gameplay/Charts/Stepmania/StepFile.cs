using System;
using System.Collections.Generic;
using System.Linq;
using System.IO;
using System.Text.RegularExpressions;
using System.Globalization;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Charts.Stepmania
{
    //sm file format sux and if stuff breaks it's not really my fault
    public class StepFile
    {
        //stores an individual difficulty/chart for a song
        public class StepFileDifficulty
        {
            public List<Measure> measures;
            public string gamemode;
            public string name;

            public StepFileDifficulty(string raw)
            {
                measures = new List<Measure>();
                string[] split = raw.Split(':');
                gamemode = split[0].Trim();
                name = split[2].Trim() + " " + split[3].Trim();
                foreach (string s in split[5].Trim().Split(',')) //stupid hack that can likely fail with some oddly formatted .sm files
                {
                    measures.Add(new Measure(s.Trim().Split('\n')));
                }
            }
        }

        public string filename;
        protected readonly string path;
        public Dictionary<string, string> Metadata;
        public List<StepFileDifficulty> Difficulties;

        public StepFile(string filename, string path)
        {
            this.filename = filename;
            this.path = path;
            Metadata = new Dictionary<string, string>();
            Difficulties = new List<StepFileDifficulty>();

            Load();
        }

        //loads file from filepath
        private void Load()
        {
            var ts = new StreamReader(Path.Combine(path, filename));
            //basically a simple parsing algorithm
            string[] l = {"", ""};
            char c; //char we're looking at
            byte state = 2; //what we're reading (key or value, etc)
            bool commentFlag = false;

            while (!ts.EndOfStream)
            {
                c = (char)ts.Read();
                if (c == '/')
                {
                    //handle comments (covers most cases)
                    //todo: more robust handling
                    if (commentFlag)
                    {
                        ts.ReadLine();
                        commentFlag = false;
                    }
                    else
                    {
                        commentFlag = true;
                    }
                }
                else
                {
                    //this is a key
                    if (c == '#')
                    {
                        state = 0;
                    }
                    //start of a value after a key
                    else if (c == ':' && state == 0)
                    {
                        state = 1;
                    }
                    //end of key:value pair
                    else if (c == ';')
                    {
                        //if the key was "NOTES" it's a difficulty, store it as such
                        if (l[0] == "NOTES")
                        {
                            Difficulties.Add(new StepFileDifficulty(l[1]));
                        }
                        else //otherwise store in the metadata dictionary
                        {
                            Metadata.Add(l[0], l[1]);
                        }
                        l = new[] { "", "" }; //prepare to parse new key:value
                        state = 2;
                    }
                    //if no special characters, keep reading the string into the right place
                    else if (state < 2)
                    {
                        l[state] += c;
                    }
                    //forget about comments if there was just a / on its own
                    commentFlag = false;
                }
            }
        }

        public string GetTag(string id)
        {
            return Metadata.ContainsKey(id) ? Metadata[id] : "This file has broken tags!!! >:(";
            //dumbass stepmania charters missing important metadata
        }

        //tries to figure out who made the chart because NOONE FORMATS THIS CONSISTENTLY
        public string GetCreator()
        {
            if (Metadata.ContainsKey("CREDIT")) //check if credit is set
            {
                if (Metadata["CREDIT"] != "")
                {
                    return Metadata["CREDIT"]; //if so use that
                }
            }

            //if not, regex the damn folder name in the hopes it's "Song name (Charter)" format which is quite common

            Regex r = new Regex(@"\(.*?\)"); //cheers to https://www.codeproject.com/Questions/296435/How-do-I-extract-a-string-of-text-that-lies-betwee
            string temp = Path.GetFileNameWithoutExtension(path);
            MatchCollection matches = r.Matches(temp);
            if (matches.Count > 0)
            {
                temp = matches[matches.Count - 1].Value; //reuse of temp cause i'm lazy
                return temp.Substring(1, temp.Length - 2); //cut off the brackets
            }
            //if all that failed, give up
            return "-----";
        }

        public string GetBG() //sm files are dumb as fuck and sm basically looks around for the bg for you
        {
            string bgfile = Metadata["BACKGROUND"] == "" ? Metadata["TITLE"] + "-bg.jpg" : Metadata["BACKGROUND"]; //if background is not set, try "Song title-bg.jpg"
            //if this file doesn't exist the bg is otherwise invalid, root around the folder manually to find something named "bg" or "background"
            //i hate my life
            if (!File.Exists(Path.Combine(path, bgfile)))
            {
                foreach (string s in Directory.GetFiles(path))
                {
                    if (Path.GetFileNameWithoutExtension(s).ToLower().Contains("bg") || Path.GetFileNameWithoutExtension(s).ToLower().Contains("background"))
                    {
                        return Path.GetFileName(s);
                    }
                }
            }
            return bgfile;
        }

        //Converts all difficulties in this file to a list of Charts
        public List<Chart> Convert()
        {
            List<Chart> charts = new List<Chart>();
            List<Tuple<double, double>> bpms = new List<Tuple<double, double>>(); //bpm data for the charts
            string[] split;

            //this parses the bpms value into meaningful data
            foreach (string s in new string(Metadata["BPMS"].Where((c) => { return !char.IsWhiteSpace(c); }).ToArray()).Split(',')) //removes all whitespace, splits by ,
            {
                split = s.Split('='); //then splits these comma separated strings by = to get beat:bpm
                bpms.Add(new Tuple<double, double>(double.Parse(split[0], CultureInfo.InvariantCulture), 60000/double.Parse(split[1], CultureInfo.InvariantCulture))); //parses bpms and puts them in this list format
            }

            //turns gamemode into key count
            foreach (StepFileDifficulty diff in Difficulties)
            {
                byte keycount;

                //https://github.com/etternagame/etterna/blob/master/src/GameManager.cpp actual list is here
                switch (diff.gamemode)
                {
                    case "dance-threepanel":
                        keycount = 3;
                        break;
                    case "dance-single":
                        keycount = 4;
                        break;
                    case "pump-single":
                        keycount = 5;
                        break;
                    case "dance-solo":
                    case "pump-halfdouble":
                        keycount = 6;
                        break;
                    case "kb7-single":
                        keycount = 7;
                        break;
                    case "dance-double":
                    case "dance-couple":
                        keycount = 8;
                        break;
                    case "pump-double":
                    case "pump-couple":
                        keycount = 10;
                        break;
                    default:
                        Utilities.Logging.Log("SM gamemode not supported: " + diff.gamemode, "", Utilities.Logging.LogType.Warning);
                        continue;
                }
                
                try
                {
                    byte meter = 4; //sm only supports 4 beats in a bar. to add ssc support take this from the file
                    List<Snap> states = new List<Snap>();
                    List<BPMPoint> points = new List<BPMPoint>();
                    BinarySwitcher lntracker = new BinarySwitcher(0);
                    double now = -double.Parse(Metadata["OFFSET"], CultureInfo.InvariantCulture) * 1000; //start time (in ms) into audio file for first measure
                    int bpm = 0; //index of bpm point for the list of tuples we generated earlier
                    points.Add(new BPMPoint((float)now, meter, (float)bpms[0].Item2)); //convert the first bpm to a timing point
                    int totalbeats = 0; //beat counter
                    double from, to;

                    for (int i = 0; i < diff.measures.Count; i++) //iterate through measures
                    {
                        totalbeats += meter; //stores what beat we arrive at at the END of this measure
                        from = 0;
                        while (bpm < bpms.Count - 1 && bpms[bpm + 1].Item1 < totalbeats) //iterate through all BPM changes within this measure
                        {
                            to = bpms[bpm + 1].Item1 - totalbeats + meter; //finds number between 0 and 4 (meter = 4)
                            diff.measures[i].ConvertSection(now, bpms[bpm].Item2, lntracker, keycount, from, to, meter, states); //correctly converts this slice
                            now += bpms[bpm].Item2 * (to - from); //updates time we're on
                            bpm += 1; //increments bpm index so we know what point we're up to
                            points.Add(new BPMPoint((float)now, meter, (float)bpms[bpm].Item2)); //adds timing point to chart
                            from = to;
                        }
                        diff.measures[i].ConvertSection(now, bpms[bpm].Item2, lntracker, keycount, from, meter, meter, states);
                        now += bpms[bpm].Item2 * (meter - from); //converts rest of measure after all bpm changes inside are complete (normally no bpm changes therefore this does the whole measure)
                    }
                    //construct chart with metadata and note data
                    Chart c = new Chart(states, new ChartHeader
                    {
                        Title = GetTag("TITLE"),
                        File = filename,
                        Artist = Metadata.ContainsKey("ARTIST") ? Metadata["ARTIST"] : GetTag("ARTISTTRANSLIT"),
                        Creator = GetCreator(),
                        SourcePath = path,
                        DiffName = (Metadata.ContainsKey("SUBTITLE") && Metadata["SUBTITLE"] != "" && Difficulties.Count == 1) ? Metadata["SUBTITLE"] : diff.name,
                        PreviewTime = float.Parse(Metadata["SAMPLESTART"], CultureInfo.InvariantCulture) * 1000,
                        AudioFile = Metadata["MUSIC"],
                        BGFile = GetBG()
                    }, keycount);
                    //set bpm data of chart (no scroll speed changes)
                    c.Timing.SetTimingData(points);
                    charts.Add(c);
                }
                catch (Exception e)
                {
                    Utilities.Logging.Log("Could not convert SM difficulty", e.ToString(), Utilities.Logging.LogType.Error);
                }
            }
            return charts;
        }
    }
}
