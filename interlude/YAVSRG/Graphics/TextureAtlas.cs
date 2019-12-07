using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using OpenTK.Graphics.OpenGL;
using System.Drawing.Imaging;

namespace Interlude.Graphics
{
    //Collects several textures together and builds them into one large texture.
    //This prevents the GPU from switching between textures (which costs performance) as it is just using different parts of one large texture.
    //I'm not using any packing algorithms to optimally fit the textures into as small a rectangle as possible because I don't think it (currently) matters.
    public class TextureAtlas : IDisposable
    {
        //Data structure containing information about each texture before the atlas is built
        public struct SpriteData
        {
            public string Name;
            public Bitmap Bitmap;
            public int Rows;
            public int Columns;
            public bool Tiling;
        }

        List<SpriteData> Textures = new List<SpriteData>();
        Dictionary<string, Sprite> Sprites = new Dictionary<string, Sprite>();
        int Texture_ID;

        public TextureAtlas()
        {
            Bitmap white = new Bitmap(100, 100);
            using (System.Drawing.Graphics g = System.Drawing.Graphics.FromImage(white)) { g.Clear(Color.White); }
            //Dummy texture for untextured things and for invalid texture names
            AddTexture(white, "");
        }

        public Sprite GetTexture(string name)
        {
            if (Sprites.ContainsKey(name))
            {
                return Sprites[name];
            }
            return Sprites[""];
        }

        public bool HasTexture(string name)
        {
            return Sprites.ContainsKey(name);
        }

        public Sprite this[string name]
        {
            get { return GetTexture(name); }
        }

        //Adds a texture to the to-be-built list - Does not update the texture atlas directly if it has already been built.
        public void AddTexture(Bitmap bmp, string name)
        {
            AddTexture(bmp, name, 1, 1);
        }

        public void AddTexture(Bitmap bmp, string name, int rows, int columns, bool tiling = false)
        {
            Textures.Add(new SpriteData() { Bitmap = bmp, Name = name, Rows = rows, Columns = columns, Tiling = tiling});
        }

        public void AddTexture(SpriteData data)
        {
            Textures.Add(data);
        }

        //Builds the texture atlas from the list of textures provided. Sprite structures will be created to reference specific parts of the texture
        //If the texture atlas has been built before, calling this again will destroy the existing atlas and rebuild from the ONLY the new data added with AddTexture since the last build
        public void Build(bool LinearClamp)
        {
            if (Texture_ID != 0) { GL.DeleteTexture(Texture_ID); }

            int width = 0;
            int height = 0;
            int x_position = 0;
            int y_position;
            foreach (SpriteData tex in Textures)
            {
                if (tex.Tiling) continue;
                height = Math.Max(height, tex.Bitmap.Height);
                if (x_position + tex.Bitmap.Width > 16384)
                {
                    x_position = 0;
                    y_position = height;
                    height = Math.Max(height, y_position + tex.Bitmap.Height);
                }
                width = Math.Max(width, x_position + tex.Bitmap.Width);
                x_position += tex.Bitmap.Width;
            }

            Texture_ID = GL.GenTexture();
            GL.BindTexture(TextureTarget.Texture2D, Texture_ID);
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, width, height, 0, OpenTK.Graphics.OpenGL.PixelFormat.Bgra, PixelType.UnsignedByte, (IntPtr)null);
            x_position = 0;
            y_position = 0;
            int h = 0;
            foreach (SpriteData tex in Textures)
            {
                var bmp = tex.Bitmap;
                if (tex.Tiling)
                {
                    Sprites.Add(tex.Name, IO.Content.UploadTexture(bmp, tex.Columns, tex.Rows, LinearClamp));
                    bmp.Dispose();
                    GL.BindTexture(TextureTarget.Texture2D, Texture_ID);
                    continue;
                }
                h = Math.Max(h, bmp.Height);
                if (x_position + bmp.Width > 16384)
                {
                    x_position = 0;
                    y_position = h;
                    h = Math.Max(h, y_position + bmp.Height);
                }
                Sprites.Add(tex.Name, new Sprite(Texture_ID, bmp.Width, bmp.Height, tex.Columns, tex.Rows, width, height, x_position, y_position));
                BitmapData data = bmp.LockBits(new Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
                GL.TexSubImage2D(TextureTarget.Texture2D, 0, x_position, y_position, data.Width, data.Height, OpenTK.Graphics.OpenGL.PixelFormat.Bgra, PixelType.UnsignedByte, data.Scan0);
                bmp.UnlockBits(data);
                x_position += bmp.Width;
                bmp.Dispose();
            }
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Clamp);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Clamp);
            if (LinearClamp)
            {
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear);
            }
            else
            {
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Nearest);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Nearest);
            }
            Textures.Clear();
        }

        public void AddSprite(Sprite s, string name)
        {
            Sprites.Add(name, s);
        }

        public void Dispose()
        {
            if (Texture_ID != 0) { GL.DeleteTexture(Texture_ID); }
            foreach (SpriteData tex in Textures)
            {
                tex.Bitmap.Dispose();
            }
        }
    }
}
