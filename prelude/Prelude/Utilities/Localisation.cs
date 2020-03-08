using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace Prelude.Utilities
{
    class Localisation
    {
        static Dictionary<string, string> Lookup;

        public static void Load(Stream file)
        {
            Lookup = new Dictionary<string, string>();
            try
            {
                string[] s;
                using (StreamReader reader = new StreamReader(file))
                {
                    s = reader.ReadLine().Trim().Split('=');
                    Lookup.Add(s[0], s[1]);
                }
            }
            catch (Exception e)
            {
                Logging.Error("error.locale", e.ToString());
            }
        }

        public static string GetTranslation(string key)
        {
            if (Lookup.ContainsKey(key)) return Lookup[key]; return key;
        }
    }
}
