using System;
using System.Reflection;
using System.IO;

namespace Interlude.IO
{
    //todo: GetFile to reduce code duplication
    public class ResourceGetter
    {
        static readonly string[] menu = LoadSplashes("Interlude.Resources.MenuSplashes.txt");
        static readonly string[] loading = LoadSplashes("Interlude.Resources.LoadingSplashes.txt");
        static readonly string[] crash = LoadSplashes("Interlude.Resources.CrashSplashes.txt");

        static Random random = new Random();

        static string[] LoadSplashes(string resourceName)
        {
            return GetResource(resourceName).Split('\n');
        }

        static string RandomSplash(string[] splash)
        {
            return splash[random.Next(0, splash.Length)];
        }

        static string GetResource(string path)
        {
            using (Stream stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(path))
            using (StreamReader reader = new StreamReader(stream))
            {
                return reader.ReadToEnd();
            }
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
            return GetResource("Interlude.Resources.Shaders." + name);
        }

        public static string GetCredits()
        {
            return GetResource("Interlude.Resources.Credits.txt");
        }
    }
}
