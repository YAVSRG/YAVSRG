using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Security.Cryptography;

namespace YAVSRG.Beatmap
{
    public class Chart
    {
        public string DifficultyName;
        public float PreviewTime;
        public int Keys;
        public PointManager<Snap> Notes;
        public PointManager<BPMPoint> Timing;
        public string bgpath;
        public string path;
        private string audioFileName;

        public Chart(List<Snap> data, List<BPMPoint> timing, string diff, float prevtime, int keys, string path, string audioFileName, string bgFileName)
        {
            Keys = keys;
            Timing = new PointManager<BPMPoint>(timing);
            Notes = new PointManager<Snap>(data);
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

        public float GetDuration()
        {
            return Notes.Points[Notes.Count - 1].Offset - Notes.Points[0].Offset;
        }

        public int GetBPM()
        {
            return (int)(60000f/Timing.Points[0].MSPerBeat);
        }

        public string GetHash()
        {
            var h = SHA256.Create();
            byte[] data = new byte[8 * Notes.Count];
            int p = 0;
            foreach (Snap s in Notes.Points)
            {
                BitConverter.GetBytes((int)s.Offset).CopyTo(data,p);
                p += 4;
                data[p] = (byte)s.taps.value;
                data[p+1] = (byte)s.holds.value;
                data[p+2] = (byte)s.ends.value;
                data[p+3] = (byte)s.middles.value;
                p += 4;
            }

            float speed = float.PositiveInfinity;
            foreach (BPMPoint s in Timing.Points)
            {
                if (speed != s.ScrollSpeed)
                {
                    Array.Resize(ref data, data.Length + 8);
                    BitConverter.GetBytes((int)s.Offset).CopyTo(data, p);
                    p += 4;
                    BitConverter.GetBytes(s.ScrollSpeed).CopyTo(data, p);
                    p += 4;
                    speed = s.ScrollSpeed;
                }
            }
            return BitConverter.ToString(h.ComputeHash(data)).Replace("-", "");
        }

        public static Chart FromFile(string path)
        {
            return null; //nyi
        }
    }
}
