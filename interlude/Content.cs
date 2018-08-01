using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK;
using OpenTK.Graphics.OpenGL;
using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using ManagedBass;


namespace YAVSRG
{
    class Content
    {
        public static readonly string AssetsDir = Path.Combine(Game.WorkingDirectory, "Data", "Assets");
        static Dictionary<string, Sprite> Store = new Dictionary<string, Sprite>();
        static Dictionary<string, int> SoundStore = new Dictionary<string, int>();

        public static Sprite LoadTexture(string path, bool getColors = false) //load texture from absolute path
        {
            if (!File.Exists(path))
            {
                throw new FileNotFoundException();
            }
            Bitmap bmp = new Bitmap(path);
            if (getColors)
            {
                Utils.SetThemeColorFromBG(bmp);
            }
            return UploadTexture(bmp, 1, 1); //temp
        }

        public static Sprite FindTextureWithUV(string name, string skin)
        {
            string filename;
            foreach (string s in Directory.GetFiles(Path.Combine(AssetsDir, skin))) //lots of files in your skin folder slows this down
            {
                filename = Path.GetFileNameWithoutExtension(s);
                int ux = 1; int uy = 1;
                if (filename.StartsWith(name))
                {
                    string[] split = filename.Split(' ');
                    split = split[split.Length - 1].Split('x');
                    if (split.Length == 2)
                    {
                        int.TryParse(split[0], out ux);
                        int.TryParse(split[1], out uy);
                    }
                    //needs some way to check format isn't being abused
                    Bitmap bmp = new Bitmap(s);
                    return UploadTexture(bmp, ux, uy);
                }
            }
            return default(Sprite);
        }

        public static Sprite GetTexture(string path)
        {
            if (!Store.ContainsKey(path))
            {
                Sprite s = FindTextureWithUV(path, Game.Options.Profile.Skin);
                if (s.Height == 0)
                {
                    s = FindTextureWithUV(path, "_fallback");
                }
                Store.Add(path, s);
            }
            return Store[path];
        }

        public static Options.Theme LoadThemeData(string name)
        {
            string newpath = Path.Combine(AssetsDir, name, "skin.json");
            Options.Theme t;
            if (!File.Exists(newpath))
            {
                t = new Options.Theme();
            }
            else
            {
                t = Utils.LoadObject<Options.Theme>(newpath);
            }
            Utils.SaveObject(t, newpath);
            t.Gameplay = LoadWidgetData(name, "gameplay");
            return t;
        }

        public static Options.WidgetPositionData LoadWidgetData(string theme, string name)
        {
            try
            {
                string path = Path.Combine(AssetsDir, theme, name + ".json");
                if (File.Exists(path)) //attempt to find and load in current assets folder
                {
                    var d = Utils.LoadObject<Options.WidgetPositionData>(path);
                    if (d != null) { return d; }
                }
                path = Path.Combine(AssetsDir, "_fallback", name + ".json");
                if (File.Exists(path)) //attempt to find and load in fallback assets folder
                {
                    var d = Utils.LoadObject<Options.WidgetPositionData>(path);
                    if (d != null) { return d; }
                }
            }
            catch
            {
                Utilities.Logging.Log("Could not load widget position data: " + name + " !", Utilities.Logging.LogType.Error);
            }
            return new Options.WidgetPositionData(); //return blank data
        }

        public static int LoadSoundFromAssets(string path)
        {
            if (!SoundStore.ContainsKey(path))
            {
                string newpath = Path.Combine(AssetsDir, Game.Options.Profile.Skin, path + ".wav");
                if (!File.Exists(newpath))
                {
                    newpath = Path.Combine(AssetsDir, "_fallback", path + ".wav");
                }
                SoundStore.Add(path, Bass.SampleLoad(newpath, 0, 0, 65535, BassFlags.AutoFree));
            }
            return SoundStore[path];
        }

        public static void ClearStore()
        {
            foreach (string k in Store.Keys)
            {
                UnloadTexture(Store[k]);
            }
            foreach (string k in SoundStore.Keys)
            {
                Bass.SampleFree(SoundStore[k]);
            }
            Store = new Dictionary<string, Sprite>();
            SoundStore = new Dictionary<string, int>();
        }

        public static Sprite LoadBackground(string path, string filename)
        {
            string e = Path.GetExtension(filename).ToLower();
            bool valid = (e == ".png" || e == ".jpg");
            if (valid && File.Exists(Path.Combine(path, filename))) return LoadTexture(Path.Combine(path, filename), true);
            Game.Screens.ChangeThemeColor(Game.Options.Theme.ThemeColor);
            return GetTexture("background");
        }

        public static Sprite UploadTexture(Bitmap bmp, int ux, int uy, bool font = false)
        {
            int id = GL.GenTexture();
            GL.BindTexture(TextureTarget.Texture2D, id);
            BitmapData data = bmp.LockBits(new Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, data.Width, data.Height, 0, OpenTK.Graphics.OpenGL.PixelFormat.Bgra, PixelType.UnsignedByte, data.Scan0);

            bmp.UnlockBits(data);

            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat);
            if (font)
            {
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear); //looks smooth/textlike when upscaling
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear);
            }
            else
            {
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Nearest); //looks sharp when upscaling
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Nearest);
            }

            return new Sprite(id, bmp.Width, bmp.Height, ux, uy);
        }

        public static void UnloadTexture(Sprite s)
        {
            GL.DeleteTexture(s.ID);
        }
    }
}
