using System.Collections.Generic;
using System.IO;
using System.Globalization;

namespace Prelude.Gameplay.Charts.Osu
{
    public class BeatmapHeader //represents those big blocks of data like [GENERAL]
    {
        private Dictionary<string, string> data;

        public float GetNumber(string key) //parses and retrieves a number
        {
            return float.Parse(data[key], CultureInfo.InvariantCulture);
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
                if (l.Trim() == "") //headers are separated by blank lines so this is how we know we got to the end
                {
                    return;
                }
                parts = l.Split(new char[] { ':' }, 2);
                try
                {
                    data.Add(parts[0], parts.Length > 1 ? parts[1].Trim() : "");
                }
                catch
                {
                    Utilities.Logging.Log("Malformed .osu header? " + l, "", Utilities.Logging.LogType.Error);
                }
            }
        }

        public void Dump(TextWriter fs)
        {
            //stub. this will write the header to a text file to save .osu chart data
        }
    }
}
