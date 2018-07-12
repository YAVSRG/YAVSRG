using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace YAVSRG.Charts.Osu
{
    public class BeatmapHeader //represents those big blocks of data like [GENERAL]
    {
        private Dictionary<string, string> data;

        public float GetNumber(string key) //parses and retrieves a number
        {
            return float.Parse(data[key]);
        }

        public string GetValue(string key) //retrieves a piece of text
        {
            return data[key];
        }

        public void SetNumber(string key, float value) //assigns a number to a key
        {
            SetValue(key, value.ToString());
        }

        public void SetValue(string key, string value) //assigns a value to a key
        {
            if (data.ContainsKey(key))
            {
                data[key] = value;
            }
            else
            {
                data.Add(key, value);
            }
        }

        public BeatmapHeader(TextReader fs) //reads from a text file
        {
            data = new Dictionary<string, string>();
            string l;
            string[] parts;
            while (true)
            {
                l = fs.ReadLine();
                if (l == "") //headers are separated by blank lines so this is how we know we got to the end
                {
                    return;
                }
                parts = l.Split(new char[] { ':' }, 2);
                data.Add(parts[0], parts[1].Trim());
            }
        }

        public void Dump(TextWriter fs)
        {
            //stub. this will write the header to a text file to save .osu chart data
        }
    }
}
