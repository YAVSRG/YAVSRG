using System;
using System.Reflection;
using System.IO;

namespace Interlude.IO
{
    //todo: GetFile to reduce code duplication
    public class ResourceGetter
    {
        static readonly string[] menu = LoadText("Interlude.Resources.MenuSplashes.txt");
        static readonly string[] loading = LoadText("Interlude.Resources.LoadingSplashes.txt");
        static readonly string[] crash = LoadText("Interlude.Resources.CrashSplashes.txt");

        static Random random = new Random();

        public static string[] LoadText(string resourceName)
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
