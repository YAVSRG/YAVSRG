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
        public SVManager Timing;

        public Chart(List<Snap> data, ChartHeader header, byte keys)
        {
            Data = header;
            Keys = keys;
            Timing = new SVManager(Keys); //put data in here after constructor
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
            if (Notes.Points.Count == 0 || Timing.BPM.Points.Count == 0) { return 120; }
            return (int)(60000f / Timing.BPM.Points[0].MSPerBeat); //todo: min and max
        }

        public string GetHash()
        {
            var h = SHA256.Create();
            byte[] data = new byte[16 * Notes.Count];
            float offset;
            if (Notes.Count > 0) { offset = Notes.Points[0].Offset; } else { return "_"; } //no hash if empty chart
            int p = 0;
            foreach (Snap s in Notes.Points)
            {
                BitConverter.GetBytes((int)(s.Offset - offset)).CopyTo(data, p);
                p += 4;
                for (int i = 0; i < 6; i++)
                {
                    BitConverter.GetBytes(s[i].value).CopyTo(data, p + 2 * i);
                }
                p += 12;
            }

            for (int i = 0; i < Timing.SV.Length; i++)
            {
                float speed = 1f;
                foreach (SVPoint sv in Timing.SV[i].Points)
                {
                    if (speed != sv.ScrollSpeed)
                    {
                        Array.Resize(ref data, data.Length + 8);
                        BitConverter.GetBytes((int)(sv.Offset - offset)).CopyTo(data, p);
                        p += 4;
                        BitConverter.GetBytes(sv.ScrollSpeed).CopyTo(data, p);
                        p += 4;
                        speed = sv.ScrollSpeed;
                    }
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
            file.Write(header); //write keycount and header. keys is not in the header because the header is not in ChartWithModifiers (chart after mods are applied in memory) and keys is needed

            file.Write(Notes.Count); //possible todo: add method to OffsetPoint to read and write bytes, then method to PointManager to read and write (cleans up this code neatly)
            foreach (Snap s in Notes.Points) //write all notes to file
            {
                file.Write(s.Offset);
                s.WriteToFile(file);
            }
            file.Write(Timing.BPM.Count);
            foreach (BPMPoint p in Timing.BPM.Points) //write all bpms to file
            {
                file.Write(p.Offset);
                file.Write(p.Meter);
                file.Write(p.MSPerBeat);
            }
            for (int i = -1; i < Keys; i++) //write all SV "channels" to file; -1 is overall scroll mult, others are column independent multipliers
            {
                file.Write(Timing.SV[i + 1].Count);
                foreach (SVPoint p in Timing.SV[i + 1].Points)
                {
                    file.Write(p.Offset);
                    file.Write(p.ScrollSpeed);
                }
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
                List<SVPoint> sv;
                Chart chart;
                using (var fs = new FileStream(path, FileMode.Open))
                {
                    var file = new BinaryReader(fs);
                    keys = file.ReadByte();
                    header = Newtonsoft.Json.JsonConvert.DeserializeObject<ChartHeader>(file.ReadString()); //it's length prefixed so it knows where to stop
                    header.File = Path.GetFileName(path); //set for use later (not loaded from the header in the file)

                    notes = new List<Snap>();
                    timing = new List<BPMPoint>();
                    int c = file.ReadInt32();
                    for (int i = 0; i < c; i++)
                    {
                        notes.Add(new Snap(file.ReadSingle()).ReadFromFile(file));
                    }
                    chart = new Chart(notes, header, keys);
                    c = file.ReadInt32();
                    for (int i = 0; i < c; i++)
                    {
                        timing.Add(new BPMPoint(file.ReadSingle(), file.ReadInt32(), file.ReadSingle()));
                    }
                    chart.Timing.SetTimingData(timing);
                    for (int lane = -1; lane < keys; lane++)
                    {
                        sv = new List<SVPoint>();
                        c = file.ReadInt32();
                        for (int i = 0; i < c; i++)
                        {
                            sv.Add(new SVPoint(file.ReadSingle(), file.ReadSingle()));
                        }
                        chart.Timing.SetSVData(lane, sv);
                    }
                    file.Close();
                }
                return chart;
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
