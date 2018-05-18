using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG
{
    class Utils
    {
        public static int Modulus(int a, int b)
        {
            while (a < 0)
            {
                a += b;
            }
            return a % b;
        }

        public static string RoundNumber(double x)
        {
            return string.Format("{0:0.00}", Math.Round(x, 2));
        }

        public static string FormatFirstCharacter(string s)
        {
            if (s.Length == 0) return "?";
            char c = s[0];
            if (char.IsLetterOrDigit(c)) { return c.ToString().ToUpper(); }
            return "?";
        }

        public static void SetDiscordData(string main, string detail)
        {
            try
            {
                Discord.UpdatePresence(new Discord.RichPresence() { state = main, details = detail });
            }
            catch { }
        }

        public static string FormatTime(float ms)
        {
            int seconds = (int)(ms / 1000) % 60;
            int minutes = (int)Math.Floor(ms % 3600000 / 60000);
            int hours = (int)Math.Floor(ms / 3600000);
            return hours > 0 ? hours.ToString() + ":" +minutes.ToString().PadLeft(2,'0') + ":" + seconds.ToString().PadLeft(2, '0') : minutes.ToString() + ":" + seconds.ToString().PadLeft(2, '0');
        }

        public static T LoadObject<T>(string path)
        {
            return Newtonsoft.Json.JsonConvert.DeserializeObject<T>(System.IO.File.ReadAllText(path));
        }

        public static void SaveObject<T>(T obj, string path)
        {
            System.IO.File.WriteAllText(path, Newtonsoft.Json.JsonConvert.SerializeObject(obj, Newtonsoft.Json.Formatting.Indented));
        }

        public static Bitmap CaptureScreen(Rectangle screenSize)
        {
            Bitmap target = new Bitmap(screenSize.Width, screenSize.Height);
            using (Graphics g = Graphics.FromImage(target))
            {
                g.CopyFromScreen(0, 0, 0, 0, new Size(screenSize.Width, screenSize.Height));
            }
            return target;
        }

        public static Color ColorInterp(Color a, Color b, float val1)
        {
            float val2 = 1 - val1;
            return Color.FromArgb((int)(a.A * val2 + b.A * val1), (int)(a.R * val2 + b.R * val1), (int)(a.G * val2 + b.G * val1), (int)(a.B * val2 + b.B * val1));
        }

        public static void SetThemeColor(Bitmap bitmap)
        {
            int goodness = 0;
            Color best = Color.White;
            for (int x = 0; x < bitmap.Width / 10; x++)
            {
                for (int y = 0; y < bitmap.Height / 10; y++)
                {
                    Color c = bitmap.GetPixel(x * 10, y * 10);
                    int compare = Math.Abs(c.R - c.B) + Math.Abs(c.B - c.G) + Math.Abs(c.G - c.R);
                    if (compare > goodness)
                    {
                        goodness = compare;
                        best = c;
                    }
                }
            }
            Game.Screens.DarkColor.Target(ColorInterp(best, Color.Black, 0.5f));
            Game.Screens.BaseColor.Target(best);
            Game.Screens.HighlightColor.Target(ColorInterp(best, Color.White, 0.5f));
        }
    }
}
