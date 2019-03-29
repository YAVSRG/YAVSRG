using System;
using System.Drawing;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Interlude
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

        public static string FormatFirstCharacter(string s) //used for grouping by first letter (always capital letter / question mark for non alphanumeric)
        {
            if (s.Length == 0) return "?";
            char c = s[0];
            if (char.IsLetterOrDigit(c)) { return c.ToString().ToUpper(); }
            return "?";
        }

        public static string FormatTime(float ms)
        {
            if (ms < 0) return "0:00"; //fix for "time left" saying "-1:00"
            int seconds = (int)(ms / 1000) % 60;
            int minutes = (int)Math.Floor(ms % 3600000 / 60000);
            int hours = (int)Math.Floor(ms / 3600000);
            return hours > 0 ? hours.ToString() + ":" +minutes.ToString().PadLeft(2,'0') + ":" + seconds.ToString().PadLeft(2, '0') : minutes.ToString() + ":" + seconds.ToString().PadLeft(2, '0');
        }

        public static T LoadObject<T>(string path) //reads an object of a given type from a file (this is how most data is stored and loaded)
        {
            return Newtonsoft.Json.JsonConvert.DeserializeObject<T>(System.IO.File.ReadAllText(path));
        }

        public static void SaveObject<T>(T obj, string path) //saves an object to a file
        {
            if (obj == null) return;
            System.IO.File.WriteAllText(path, Newtonsoft.Json.JsonConvert.SerializeObject(obj, Newtonsoft.Json.Formatting.Indented));
        }

        public static Bitmap CaptureDesktop(Rectangle screenSize) //this is only used when the game starts to create the "fade in effect" (you can't have translucent windows)
        {
            Bitmap target = new Bitmap(screenSize.Width, screenSize.Height);
            using (System.Drawing.Graphics g = System.Drawing.Graphics.FromImage(target))
            {
                g.CopyFromScreen(0, 0, 0, 0, new Size(screenSize.Width, screenSize.Height));
            }
            return target;
        }

        public static Bitmap CaptureWindow() //this is used to screenshot the game. it pulls the screen pixels from the GPU because this always works (other methods dont work for fullscreen)
        {
            Bitmap target = new Bitmap(Game.Instance.Width, Game.Instance.Height);
            System.Drawing.Imaging.BitmapData data = target.LockBits(new Rectangle(0, 0, Game.Instance.Width, Game.Instance.Height), System.Drawing.Imaging.ImageLockMode.WriteOnly, System.Drawing.Imaging.PixelFormat.Format24bppRgb);
            OpenTK.Graphics.OpenGL.GL.ReadPixels(0, 0, Game.Instance.Width, Game.Instance.Height, OpenTK.Graphics.OpenGL.PixelFormat.Bgr, OpenTK.Graphics.OpenGL.PixelType.UnsignedByte, data.Scan0);
            target.UnlockBits(data);
            target.RotateFlip(RotateFlipType.Rotate180FlipX); //the data comes out upside down and rotated
            return target;
        }

        public static float GetBeat(int i)
        {
            float t = (float)Game.Audio.Now();
            BPMPoint p = Game.Gameplay.CurrentChart.Timing.BPM.GetPointAt(t, false);
            return (float)Math.Cos(((t - p.Offset) / (p.MSPerBeat * i)) % 1 * Math.PI * 2);
        }

        public static Color ColorInterp(Color a, Color b, float val1) //merges between two colors. 0 just gives a, 1 just gives b, 0.5 is exactly in between (in R G and B)
        {
            float val2 = 1 - val1;
            return Color.FromArgb((int)(a.A * val2 + b.A * val1), (int)(a.R * val2 + b.R * val1), (int)(a.G * val2 + b.G * val1), (int)(a.B * val2 + b.B * val1));
        }

        public static void SetThemeColorFromBG(Bitmap bitmap) //algorithm to pick nice theme color from bg
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
            if (goodness > 127) //goodness measures how vibrant the color is. if less than 127 it's likely a shade of grey/black/white so the default color is used instead
            {
                Game.Screens.ChangeThemeColor(Color.FromArgb(255,best));
            }
            else
            {
                Game.Screens.ChangeThemeColor(Game.Options.Theme.DefaultThemeColor);
            }
        }
    }
}
