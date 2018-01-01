using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace YAVSRG.Beatmap
{
    public class BeatmapHeader
    {
        private Dictionary<string, string> data;

        public float GetNumber(string key)
        {
            return float.Parse(data[key]);
        }

        public string GetValue(string key)
        {
            return data[key];
        }

        public void SetNumber(string key, float value)
        {
            SetValue(key, value.ToString());
        }

        public void SetValue(string key, string value)
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

        public BeatmapHeader(TextReader fs)
        {
            data = new Dictionary<string, string>();
            string l;
            string[] parts;
            while (true)
            {
                l = fs.ReadLine();
                if (l == "")
                {
                    return;
                }
                parts = l.Split(new char[] { ':' }, 2);
                data.Add(parts[0], parts[1].Trim());
            }
        }

        public void Dump(TextWriter fs)
        {

        }
    }
}
