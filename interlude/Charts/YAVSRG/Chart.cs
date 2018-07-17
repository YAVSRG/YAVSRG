using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Security.Cryptography;
using System.IO;

namespace YAVSRG.Charts.YAVSRG
{
    public class Chart
    {
        public ChartHeader Data;
        public byte Keys; //keycount is separate from the header to keep it consistent with the "Chart" instance for gameplay, which has no header, just notes.
        public PointManager<Snap> Notes;
        public PointManager<BPMPoint> Timing;

        public Chart(List<Snap> data, List<BPMPoint> timing, ChartHeader header, byte keys)
        {
            Data = header;
            Keys = keys;
            Timing = new PointManager<BPMPoint>(timing);
            Notes = new PointManager<Snap>(data);
        }

        public string AudioPath()
        {
            return Path.Combine(Data.SourcePath, Data.AudioFile);
        }

        public float GetDuration()
        {
            if (Notes.Points.Count == 0) { return 0; }
            return Notes.Points[Notes.Count - 1].Offset - Notes.Points[0].Offset;
        }

        public int GetBPM()
        {
            if (Notes.Points.Count == 0) { return 120; }
            return (int)(60000f / Timing.Points[0].MSPerBeat);
        }

        public string GetHash()
        {
            var h = SHA256.Create();
            byte[] data = new byte[8 * Notes.Count];
            float offset;
            if (Notes.Count > 0) { offset = Notes.Points[0].Offset; } else { return "_"; } //no hash if empty chart
            int p = 0;
            foreach (Snap s in Notes.Points)
            {
                BitConverter.GetBytes((int)(s.Offset - offset)).CopyTo(data, p);
                p += 4;
                data[p] = (byte)s.taps.value;
                data[p + 1] = (byte)s.holds.value;
                data[p + 2] = (byte)s.ends.value;
                data[p + 3] = (byte)s.middles.value;
                p += 4;
            }

            float speed = float.PositiveInfinity;
            foreach (BPMPoint s in Timing.Points)
            {
                if (speed != s.ScrollSpeed)
                {
                    Array.Resize(ref data, data.Length + 8);
                    BitConverter.GetBytes((int)(s.Offset - offset)).CopyTo(data, p);
                    p += 4;
                    BitConverter.GetBytes(s.ScrollSpeed).CopyTo(data, p);
                    p += 4;
                    speed = s.ScrollSpeed;
                }
            }
            return BitConverter.ToString(h.ComputeHash(data)).Replace("-", "");
        }

        public void WriteToFile(string path)
        {
            string header = Newtonsoft.Json.JsonConvert.SerializeObject(Data);
            var fs = new FileStream(path, FileMode.Create);
            var file = new BinaryWriter(fs);
            file.Write(Keys);
            file.Write(header);
            file.Write(Notes.Count);
            foreach (Snap s in Notes.Points)
            {
                file.Write(s.Offset);
                file.Write(s.taps.value); file.Write(s.holds.value); file.Write(s.middles.value); file.Write(s.ends.value); file.Write(s.mines.value);
            }
            file.Write(Timing.Count);
            foreach (BPMPoint p in Timing.Points)
            {
                file.Write(p.Offset);
                file.Write(p.Meter);
                file.Write(p.MSPerBeat);
                file.Write(p.ScrollSpeed);
                file.Write(p.InheritsFrom);
            }
            file.Close();
            fs.Close();
        }

        public static Chart FromFile(string path)
        {
            try
            {
                byte keys;
                ChartHeader header;
                List<Snap> notes;
                List<BPMPoint> timing;
                using (var fs = new FileStream(path, FileMode.Open))
                {
                    var file = new BinaryReader(fs);
                    keys = file.ReadByte();
                    header = Newtonsoft.Json.JsonConvert.DeserializeObject<ChartHeader>(file.ReadString()); //it's length prefixed so it knows where to stop
                    header.File = Path.GetFileName(path); //in case you rename the file
                    notes = new List<Snap>();
                    timing = new List<BPMPoint>();
                    int c = file.ReadInt32();
                    for (int i = 0; i < c; i++)
                    {
                        notes.Add(new Snap(file.ReadSingle(), file.ReadUInt16(), file.ReadUInt16(), file.ReadUInt16(), file.ReadUInt16(), file.ReadUInt16()));
                    }
                    c = file.ReadInt32();
                    for (int i = 0; i < c; i++)
                    {
                        timing.Add(new BPMPoint(file.ReadSingle(), file.ReadInt32(), file.ReadSingle(), file.ReadSingle(), file.ReadSingle()));
                    }
                    file.Close();
                }
                return new Chart(notes, timing, header, keys);
            }
            catch
            {
                Utilities.Logging.Log("Could not load chart at " + path, Utilities.Logging.LogType.Error);
                return null;
            }
        }
        
        public string GetFileIdentifier()
        {
            return Path.Combine(Data.SourcePath, Data.File);
        }
    }
}
