using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap
{
    public class Chart
    {
        public string DifficultyName;
        public float PreviewTime;
        public int Keys;
        public PointManager<Snap> States;
        public PointManager<BPMPoint> Timing;
        private string bgpath;
        private Sprite bg;
        public string path;
        private string audioFileName;

        public Sprite background
        {
            get { if (bg.Height == 0) { bg = Content.LoadBackground(path, bgpath); } return bg; }
        }


        public Chart(List<Snap> data, List<BPMPoint> timing, string diff, float prevtime, int keys, string path, string audioFileName, string bgFileName)
        {
            Keys = keys;
            Timing = new PointManager<BPMPoint>(timing);
            States = new PointManager<Snap>(data);
            DifficultyName = diff;
            PreviewTime = prevtime;
            this.path = path;
            this.audioFileName = audioFileName;
            bgpath = bgFileName;
        }

        public string AudioPath()
        {
            return System.IO.Path.Combine(path, audioFileName);
        }

        public void UnloadBackground()
        {
            Content.UnloadTexture(bg);
            bg.Height = 0;
        }

        public float GetDuration()
        {
            return States.Points[States.Count - 1].Offset - States.Points[0].Offset;
        }

        public int GetBPM()
        {
            return (int)(60000f/Timing.Points[0].MSPerBeat);
        }

        public static Chart FromFile(string path)
        {
            return null; //nyi
        }
    }
}
