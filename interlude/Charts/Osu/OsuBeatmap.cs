using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Charts.Osu
{
    public class OsuBeatmap
    {
        public string filename;
        protected readonly string path;

        public ChartsHeader General;
        public ChartsHeader Editor;
        public ChartsHeader Metadata;
        public ChartsHeader Difficulty;

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
                    General = new ChartsHeader(ts);
                }
                else if (l == "[Editor]")
                {
                    Editor = new ChartsHeader(ts);
                }
                else if (l == "[Metadata]")
                {
                    Metadata = new ChartsHeader(ts);
                }
                else if (l == "[Difficulty]")
                {
                    Difficulty = new ChartsHeader(ts);
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
            Chart c = new Chart(hitdata, TimingPoints.Convert(hitdata[hitdata.Count - 1].Offset), new ChartHeader
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
            return c;
        }
    }
}
