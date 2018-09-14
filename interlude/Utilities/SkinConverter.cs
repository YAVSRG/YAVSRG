using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO;

namespace YAVSRG.Utilities
{
    public class SkinConverter
    {
        /*
        static void TryCopy(string source, string sourceDir, string target, string targetDir)
        {
            foreach (string file in Directory.EnumerateFiles(sourceDir))
            {
                if (Path.GetFileNameWithoutExtension(file).Contains(source))
                {
                    string suffix = Path.GetFileNameWithoutExtension(file).Split(new string[] { source }, StringSplitOptions.None)[1];
                    string dimensions = suffix.Length >= 4 ? " "+suffix.Substring(1).Split(' ')[0] : "";
                    Console.WriteLine(target + dimensions + Path.GetExtension(file));
                    return;
                }
            }
        }

        static void TryCopyWithTextureStitching(string source, string sourceDir, string target, string targetDir)
        {

        }

        public static void ConvertFromStepmania(string path)
        {
            TryCopy("tap note", @"C:\Games\Etterna\NoteSkins\dance\Percyqaz", "note", "");
        }*/
    }
}
