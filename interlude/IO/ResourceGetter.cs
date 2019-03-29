using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using System.IO;

namespace Interlude.IO
{
    public class ResourceGetter
    {
        static readonly string[] menu = LoadSplashes("YAVSRG.Resources.MenuSplashes.txt");
        static readonly string[] loading = LoadSplashes("YAVSRG.Resources.LoadingSplashes.txt");
        static readonly string[] crash = LoadSplashes("YAVSRG.Resources.CrashSplashes.txt");

        static Random random = new Random();

        static string[] LoadSplashes(string resourceName)
        {
            using (Stream stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName))
            using (StreamReader reader = new StreamReader(stream))
            {
                return reader.ReadToEnd().Split('\n');
            }
        }

        static string RandomSplash(string[] splash)
        {
            return splash[random.Next(0, splash.Length)];
        }

        public static string MenuSplash()
        {
            return RandomSplash(menu);
        }

        public static string LoadingSplash()
        {
            return RandomSplash(loading);
        }

        public static string CrashSplash()
        {
            return RandomSplash(crash);
        }

        public static string GetShader(string name)
        {
            using (Stream stream = Assembly.GetExecutingAssembly().GetManifestResourceStream("YAVSRG.Resources.Shaders."+name))
            using (StreamReader reader = new StreamReader(stream))
            {
                return reader.ReadToEnd();
            }
        }

    }
}
