using System.Collections.Generic;
using System.Globalization;
using System.IO;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Charts.Osu
{
    public class HitObjectConverter //encapsulates all hitobjects in a map. it is called HitObjectConverter because that's all it does, with no tools for manipulating the hitobjects
    {
        private List<HitObject> objects;

        public HitObjectConverter(TextReader fs)
        {
            objects = new List<HitObject>();
            string l;
            while (true)
            {
                l = fs.ReadLine();
                if (l == "" || l == null)
                {
                    break;
                }
                objects.Add(new HitObject(l));
            }
        }

        public HitObjectConverter(List<Snap> s, byte k)
        {
            CreateObjectsFromSnaps(s, k);
        }

        public void Sort()
        {
            objects.Sort((a, b) => { return a.offset.CompareTo(b.offset); });
        }

        public List<Snap> CreateSnapsFromObjects(byte keys) //old algorithm, originally in python for a hobby script
        //this converts a list of hitobjects from o!m to a list of "snaps" or rows containing objects (and LN ends) in line with each other
        {
            List<Snap> states = new List<Snap>();
            Snap s = new Snap(-1);
            float[] holds = new float[keys];
            for (byte k = 0; k < keys; k++)
            {
                holds[k] = -1;
            }
            float last = -1;
            bool ln;
            float time;
            byte col;
            for (int i = 0; i < objects.Count; i++)
            {
                time = objects[i].offset;
                col = XToColumn(objects[i].x, keys);
                ln = (objects[i].type & 128) > 0;

                if (time != last) //create new state
                {
                    states.Add(s);
                    s = new Snap(time);

                    while (true) //THIS SECTION ROUNDS OFF ANY LONG NOTES THAT END BEFORE THE NEXT HIT OBJECT ARRIVES ----
                    {
                        float min = time; //This determines the earliest release of a long note between last object and now
                        for (byte k = 0; k < keys; k++)
                        {
                            if (holds[k] == -1) { continue; }
                            if (holds[k] < min) { min = holds[k]; }
                        }
                        if (min < time) //This uses the above's information to add a state where 1 or more long notes end
                        {
                            Snap temp = new Snap(min);
                            for (byte k = 0; k < keys; k++)
                            {
                                if(holds[k] == min)
                                {
                                    temp.ends.SetColumn(k);
                                    holds[k] = -1;
                                }
                                else if (holds[k] > min)
                                {
                                    temp.middles.SetColumn(k);
                                }
                            }
                            states.Add(temp);
                        }
                        else //Repeat until all long notes that end before the next object have been given an end state
                        {
                            break;
                        }
                    } // ---------------------------------------------------------------------------------------------------

                    for (byte k = 0; k < keys; k++) //Now add markers for current holds in the upcoming state, and for new LN presses.
                    {
                        if (holds[k] == -1) { continue; }
                        else if (holds[k] > time)
                        {
                            s.middles.SetColumn(k); //this will happen multiple times but i don't care
                        }
                        else if(holds[k] == time)
                        {
                            s.ends.SetColumn(k);
                            holds[k] = -1;
                        }
                    }
                    last = time;
                }
                if (ln) //After all this is done, add single notes or new arriving long notes to the current state
                {
                    s.holds.SetColumn(col);
                    holds[col] = float.Parse(objects[i].addition.Split(':')[0], CultureInfo.InvariantCulture);
                }
                else
                {
                    s.taps.SetColumn(col);
                }
            }
            states.Add(s); //Add the last state we missed cause no new notes were being added
            while (true) //THIS IS A REPEATED SECTION FROM ABOVE, WE ARE ROUNDING OFF ANY HELD LONG NOTES AT THEIR CORRECT TIMES
            {
                float min = 10000000;
                for (byte k = 0; k < keys; k++)
                {
                    if (holds[k] == -1) { continue; }
                    if (holds[k] < min) { min = holds[k]; }
                }
                if (min < 10000000)
                {
                    ushort end = 0;
                    ushort mid = 0;
                    for (byte k = 0; k < keys; k++)
                    {
                        if (holds[k] == min)
                        {
                            end += (ushort)(1 << k);
                            holds[k] = -1;
                        }
                        else if (holds[k] > min)
                        {
                            mid += (ushort)(1 << k);
                        }
                    }
                    states.Add(new Snap(min, 0, 0, mid, end, 0));
                }
                else
                {
                    break;
                }
            } //We are done. An extra state was added at the start, timestamp -1. remove it because I cba to make a better solution above
            states.RemoveAt(0);
            return states;
        }

        public void CreateObjectsFromSnaps(List<Snap> states, byte keys)
        //this is the reverse of the above algorithm (and untested)
        {
            objects = new List<HitObject>();
            HitObject[] ln = new HitObject[keys];
            foreach (Snap s in states)
            {
                for (byte k = 0; k < keys; k++)
                {
                    if (s.taps.GetColumn(k))
                    {
                        objects.Add(new HitObject(ColumnToX(k, keys), 0, (int)s.Offset, 1, 0, "0:0:0:0:"));
                    }
                    else if (s.holds.GetColumn(k))
                    {
                        ln[k] = new HitObject(ColumnToX(k, keys), 0, (int)s.Offset, 128, 0, "0:0:0:0:0:");
                        objects.Add(ln[k]);
                    }
                    else if (s.ends.GetColumn(k))
                    {
                        ln[k].addition = ((int)s.Offset).ToString()+":0:0:0:0:";
                    }
                }
            }
        }

        public byte XToColumn(int x, byte keys) //converts osu x coordinate to column number
        {
            return (byte)(x / (512f / keys));
        }

        public int ColumnToX(int c, byte keys) //converts column number to osu x coordinate
        {
            return (int)(c * (512f / keys));
        }

        public void Dump(TextWriter tw) //writes objects to text file
        {
            foreach (HitObject o in objects)
            {
                o.Dump(tw);
            }
        }
    }
}
