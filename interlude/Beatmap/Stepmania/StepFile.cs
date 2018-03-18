using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace YAVSRG.Beatmap.Stepmania
{
    public class StepFile
    {
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
                foreach (string s in split[5].Trim().Split(','))
                {
                    measures.Add(new Measure(s.Trim().Split('\n')));
                }
            }
        }

        public string filename;
        protected readonly string path;
        public Dictionary<string, string> raw;
        public List<StepFileDifficulty> diffs;

        public StepFile(string filename, string path)
        {
            this.filename = filename;
            this.path = path;
            raw = new Dictionary<string, string>();
            diffs = new List<StepFileDifficulty>();
            Load();
        }

        private void Load()
        {
            var ts = new StreamReader(Path.Combine(path, filename));
            string[] l = {"", ""};
            char c;
            byte state = 2;

            while (!ts.EndOfStream)
            {
                c = (char)ts.Read();
                if (c == '/' && state != 2)
                {
                    ts.ReadLine();
                }
                else if (c == '#')
                {
                    state = 0;
                }
                else if (c == ':' && state == 0)
                {
                    state = 1;
                }
                else if (c == ';')
                {
                    if (l[0] == "NOTES")
                    {
                        diffs.Add(new StepFileDifficulty(l[1]));
                    }
                    else
                    {
                        raw.Add(l[0], l[1]);
                    }
                    l = new[] {"",""};
                    state = 2;
                }
                else if (state < 2)
                {
                    l[state] += c;
                }
            }
        }

        public string GetTag(string id)
        {
            return raw.ContainsKey(id) ? raw[id] : "This file has broken tags!!! >:(";
        }

        public string GetSubtitle()
        {
            return raw.ContainsKey("CREDIT") ? (raw["CREDIT"] == "" ? (raw.ContainsKey("SUBTITLE") ? raw["SUBTITLE"] : "retard") : raw["CREDIT"]) : "actual retard fix your tags";
        }

        public string GetBG()
        {
            return raw["BACKGROUND"] == "" ? raw["TITLE"]+"-bg.jpg" : raw["BACKGROUND"];
        }

        public MultiChart ConvertToRoot()
        {
            MultiChart c = new MultiChart(new ChartHeader(GetTag("TITLE"), raw.ContainsKey("ARTIST") ? raw["ARTIST"] : GetTag("ARTISTTRANSLIT"), GetSubtitle(), path));
            c.diffs = Convert();
            if (c.diffs.Count == 0) { return null; }
            return c;
        }

        public List<Chart> Convert()
        {
            List<Chart> charts = new List<Chart>();
            List<Tuple<double, double>> bpms = new List<Tuple<double, double>>();
            string[] split;

            foreach (string s in new string(raw["BPMS"].Where((c) => { return !char.IsWhiteSpace(c); }).ToArray()).Split(','))
            {
                split = s.Split('=');
                bpms.Add(new Tuple<double, double>(double.Parse(split[0]), 60000/double.Parse(split[1])));
            }

            foreach (StepFileDifficulty diff in diffs)
            {
                if (diff.gamemode != "dance-single") { continue; }
                int meter = 4;
                List<Snap> states = new List<Snap>();
                List<BPMPoint> points = new List<BPMPoint>();
                Snap.BinarySwitcher lntracker = new Snap.BinarySwitcher(0);
                double now = -double.Parse(raw["OFFSET"]) * 1000;
                int bpm = 0;
                points.Add(new BPMPoint((float)now, meter, (float)bpms[0].Item2, 1, (float)now));
                int totalbeats = 0;
                int keycount = 4;

                for (int i = 0; i < diff.measures.Count; i++)
                {
                    for (int b = 0; b < meter; b++)
                    {
                        diff.measures[i].ConvertBeat(now, bpms[bpm].Item2, lntracker, keycount, b, meter, states);
                        now += bpms[bpm].Item2;
                        totalbeats += 1;
                        if (bpm < bpms.Count - 1 && bpms[bpm+1].Item1 <= totalbeats)
                        {
                            bpm += 1;
                            points.Add(new BPMPoint((float)now, meter, (float)bpms[bpm].Item2, 1, (float)now));
                        }
                    }
                }
                Chart c = new Chart(states, points, diff.name, float.Parse(raw["SAMPLESTART"]) * 1000, keycount, path, raw["MUSIC"], GetBG());
                charts.Add(c);
            }
            return charts;
        }
    }
}
