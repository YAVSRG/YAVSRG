using System.Drawing;
using System.Drawing.Imaging;
using System.IO;
using OpenTK.Graphics.OpenGL;
using Interlude.Graphics;


namespace Interlude.IO
{
    class Content
    {
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
            return UploadTexture(bmp, 1, 1, getColors); //temp (?)
        }

        public static Sprite LoadBackground(string path, string filename)
        {
            string e = Path.GetExtension(filename).ToLower();
            bool valid = (e == ".png" || e == ".jpg");
            if (valid && File.Exists(Path.Combine(path, filename))) return LoadTexture(Path.Combine(path, filename), true);
            Game.Screens.ChangeThemeColor(Game.Options.Theme.DefaultThemeColor);
            return Game.Options.Themes.GetTexture("background");
        }

        public static Sprite UploadTexture(Bitmap bmp, int ux, int uy, bool font = false)
        {
            int id = GL.GenTexture();
            BitmapData data = bmp.LockBits(new Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
            GL.BindTexture(TextureTarget.Texture2D, id);
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
            GL.DeleteTexture(s.GL_Texture_ID);
        }
    }
}
