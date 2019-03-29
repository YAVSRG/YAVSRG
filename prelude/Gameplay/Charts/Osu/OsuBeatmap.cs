using System.Collections.Generic;
using System.IO;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Charts.Osu
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

        public virtual byte Mode
        {
            get { return (byte)General.GetNumber("Mode"); }
        }

        public virtual byte Keys
        {
            get { return (byte)Difficulty.GetNumber("CircleSize"); }
        }

        public OsuBeatmap(string filename, string path)
        {
            this.filename = filename;
            this.path = path;
            Load();
        }

        public OsuBeatmap(Chart c)
        {
            //unfinished
            filename = Path.ChangeExtension(c.Data.File, ".osu");
            path = c.Data.SourcePath;
            HitObjects = new HitObjectConverter(c.Notes.Points, c.Keys);
            //TimingPoints = new TimingPointConverter(c.Timing.Points);
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
                }
            }
        }

        public Chart Convert()
        {
            if (Mode != 3) { return null; }
            HitObjects.Sort();
            List<Snap> hitdata = HitObjects.CreateSnapsFromObjects(Keys);
            Chart c = new Chart(hitdata, new ChartHeader
            {
                Title = Metadata.GetValue("Title"),
                Artist = Metadata.GetValue("Artist"),
                Creator = Metadata.GetValue("Creator"),
                SourcePath = path,
                File = filename,
                DiffName = Metadata.GetValue("Version"),
                PreviewTime = General.GetNumber("PreviewTime"),
                AudioFile = General.GetValue("AudioFilename"),
                BGFile = Events.GetBGPath()
            }, Keys);
            TimingPoints.Convert(hitdata[hitdata.Count - 1].Offset, c.Timing);
            return c;
        }
    }
}
