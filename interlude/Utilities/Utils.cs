using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Interface;

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
            if (ms < 0) return ""; //fix for "time left" saying "-1:00"
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

        public static Bitmap CaptureDesktop(Rectangle screenSize)
        {
            Bitmap target = new Bitmap(screenSize.Width, screenSize.Height);
            using (Graphics g = Graphics.FromImage(target))
            {
                g.CopyFromScreen(0, 0, 0, 0, new Size(screenSize.Width, screenSize.Height));
            }
            return target;
        }

        public static Bitmap CaptureWindow()
        {
            Bitmap target = new Bitmap(ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2);
            System.Drawing.Imaging.BitmapData data = target.LockBits(new Rectangle(0, 0, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2), System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
            OpenTK.Graphics.OpenGL.GL.ReadPixels(0, 0, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, OpenTK.Graphics.OpenGL.PixelFormat.Bgr, OpenTK.Graphics.OpenGL.PixelType.UnsignedByte, data.Scan0);
            target.UnlockBits(data);
            target.RotateFlip(RotateFlipType.Rotate180FlipX);
            return target;
        }

        public static float GetBeat(int i)
        {
            float t = (float)Game.Audio.Now();
            Charts.YAVSRG.BPMPoint p = Game.Gameplay.CurrentChart.Timing.GetPointAt(Game.Gameplay.CurrentChart.Timing.GetPointAt(t, false).InheritsFrom, false);
            return (float)Math.Cos(((t - p.Offset) / (p.MSPerBeat * i)) % 1 * Math.PI * 2);
        }

        public static Color ColorInterp(Color a, Color b, float val1)
        {
            float val2 = 1 - val1;
            return Color.FromArgb((int)(a.A * val2 + b.A * val1), (int)(a.R * val2 + b.R * val1), (int)(a.G * val2 + b.G * val1), (int)(a.B * val2 + b.B * val1));
        }

        public static void SetThemeColorFromBG(Bitmap bitmap)
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
            if (goodness > 127)
            {
                Game.Screens.ChangeThemeColor(best);
            }
            else
            {
                Game.Screens.ChangeThemeColor(Game.Options.Theme.ThemeColor);
            }
        }

        public static double RootMeanPower(List<double> data, float power)
        {
            if (data.Count == 0) { return 0; }
            if (data.Count == 1) { return data[0]; };
            double f = 0;
            foreach (float v in data)
            {
                f += Math.Pow(v, power);
            }
            return Math.Pow(f / data.Count, 1f / power);
        }
    }
}
