using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using YAVSRG.Beatmap.Osu;

namespace YAVSRG.Beatmap
{
    public class OsuBeatmap
    {
        public string filename;
        protected readonly string path;

        public BeatmapHeader General;
        public BeatmapHeader Editor;
        public BeatmapHeader Metadata;
        public BeatmapHeader Difficulty;

        public HitObjectConverter HitObjects;
        public TimingPointConverter TimingPoints;
        public EventData Events;

        public virtual int Mode
        {
            get { return (int)General.GetNumber("Mode"); }
        }

        public virtual int Keys
        {
            get { return (int)Difficulty.GetNumber("CircleSize"); }
        }

        public OsuBeatmap(string filename, string path)
        {
            this.filename = filename;
            this.path = path;
            Load();
        }

        private void Load()
        {
            var ts = new StreamReader(Path.Combine(path, filename));
            string l;
            while (!ts.EndOfStream)
            {
                l = ts.ReadLine();
                if (l == "[General]")
                {
                    General = new BeatmapHeader(ts);
                }
                else if (l == "[Editor]")
                {
                    Editor = new BeatmapHeader(ts);
                }
                else if (l == "[Metadata]")
                {
                    Metadata = new BeatmapHeader(ts);
                }
                else if (l == "[Difficulty]")
                {
                    Difficulty = new BeatmapHeader(ts);
                }
                else if (l == "[TimingPoints]")
                {
                    TimingPoints = new TimingPointConverter(ts);
                }
                else if (l == "[Events]")
                {
                    Events = new EventData(ts);
                }
                else if (l == "[HitObjects]")
                {
                    HitObjects = new HitObjectConverter(ts);
                    HitObjects.CreateSnapsFromObjects(Keys);
                }
            }
        }

        public MultiChart ConvertToRoot()
        {
            if (Mode != 3) { return null; }
            ChartHeader header = new ChartHeader { title = Metadata.GetValue("Title"), artist = Metadata.GetValue("Artist"), creator = Metadata.GetValue("Creator"), path = path };
            MultiChart diffs = new MultiChart(header);
            Chart c = Convert();
            diffs.diffs.Add(c);
            return diffs;
        }

        public Chart Convert()
        {
            if (Mode != 3) { return null; }
            Chart c = new Chart(HitObjects.CreateSnapsFromObjects(Keys), TimingPoints.Convert(), Metadata.GetValue("Version"), General.GetNumber("PreviewTime"), Keys, path, General.GetValue("AudioFilename"), Events.GetBGPath());
            return c;
        }
    }
}
