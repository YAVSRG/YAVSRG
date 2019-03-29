using System;
using System.Collections.Generic;
using System.Security.Cryptography;
using System.IO;

namespace Prelude.Gameplay.Charts.YAVSRG
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

        public string GetHash() //unique identifier for the content of the chart - identical charts stored in different locations can use the same score data because if two charts are the same, they have the same hash
            //it can be assumed that if two charts have the same hash, they are identical since hash collisions (the case when this is not true) is astronomically rare
        {
            var h = SHA256.Create();
            byte[] data = new byte[16 * Notes.Count]; //maybe find a way to optimise resizing of arrays
            float offset; //used to calculate note's time relative to the first note. prevents identical charts but with everything shifted in time having different hashes
            if (Notes.Count > 0) { offset = Notes.Points[0].Offset; } else { return "_"; } //no hash if empty chart

            int p = 0; //pointer variable
            foreach (Snap s in Notes.Points)
            {
                BitConverter.GetBytes((int)(s.Offset - offset)).CopyTo(data, p); //write bytes for time into the chart this row of notes is
                p += 4;
                for (int i = 0; i < 6; i++)
                {
                    BitConverter.GetBytes(s[i].value).CopyTo(data, p + 2 * i); //write bytes for each component (regular notes, start of holds, middle of holds, end of holds, mines, special; in that order)
                    //special is always 0 for now but in future will be used for scratch lanes from BMS and/or lasers from SDVX (or any other stuff i might add)
                }
                p += 12; //move pointer in byte array by 12 to put next block of data in the right place
            }

            for (int i = 0; i < Timing.SV.Length; i++)
            {
                float speed = 1f;
                foreach (SVPoint sv in Timing.SV[i].Points)
                {
                    if (speed != sv.ScrollSpeed) //only write data if the new SV line changes the scroll speed from what it was
                    {
                        Array.Resize(ref data, data.Length + 8); //array resizing is inefficiently done here since it depends on if the SV changes scroll speed or not
                        BitConverter.GetBytes((int)(sv.Offset - offset)).CopyTo(data, p);
                        p += 4;
                        BitConverter.GetBytes(sv.ScrollSpeed).CopyTo(data, p);
                        //hash is affected by both time and new scroll speed at this time
                        p += 4;
                        speed = sv.ScrollSpeed;
                    }
                }
            }
            //so in the end, data represents a compacted byte array representing the chart data (and is 100% unique for unique charts)
            return BitConverter.ToString(h.ComputeHash(data)).Replace("-", ""); //the hashing algorithm transforms this data into something only 256 bits long, in a manner where these 256 bits are (VERY) probably still unique.
            //the hash is converted to a hexadecimal string for ease of handling (although i may switch to base64 in the future)
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
                    keys = file.ReadByte(); //first byte is keycount
                    header = Newtonsoft.Json.JsonConvert.DeserializeObject<ChartHeader>(file.ReadString()); //it's length prefixed so it knows where to stop
                    header.File = Path.GetFileName(path); //set for use in memory later (not saved/loaded from the header in the file)

                    notes = new List<Snap>();
                    timing = new List<BPMPoint>();
                    int c = file.ReadInt32(); //next is 4 bytes representing number of snaps to read
                    for (int i = 0; i < c; i++)
                    {
                        notes.Add(new Snap(file.ReadSingle()).ReadFromFile(file)); //read snap from bytes
                    }
                    chart = new Chart(notes, header, keys);
                    c = file.ReadInt32(); //next is 4 bytes representing number of BPM points to read
                    for (int i = 0; i < c; i++)
                    {
                        timing.Add(new BPMPoint(file.ReadSingle(), file.ReadInt32(), file.ReadSingle())); //read bpm point data
                    }
                    chart.Timing.SetTimingData(timing);
                    for (int lane = -1; lane < keys; lane++) //there are keys+1 SV channels to read
                    {
                        sv = new List<SVPoint>();
                        c = file.ReadInt32(); //read 4 bytes for number of points
                        for (int i = 0; i < c; i++)
                        {
                            sv.Add(new SVPoint(file.ReadSingle(), file.ReadSingle())); //read that number of points into the channel
                        }
                        chart.Timing.SetSVData(lane, sv);
                    }
                    file.Close();
                }
                return chart;
            }
            catch (Exception e)
            {
                Utilities.Logging.Log("Could not load chart at " + path, e.ToString(), Utilities.Logging.LogType.Error);
                return null;
            }
        }
        
        //easy to generate/use "identifier" for where to find this chart or data for it
        public string GetFileIdentifier()
        {
            return Path.Combine(Data.SourcePath, Data.File);
        }
    }
}
