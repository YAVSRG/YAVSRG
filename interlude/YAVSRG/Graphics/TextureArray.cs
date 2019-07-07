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
    public class TextureArray
    {
        int GL_ID;
        int Counter = 0;
        int Width;
        int Height;
        int Size;
        SpriteBatch.TextureUnit Type;

        public TextureArray(int width, int height, int count, bool smooth, SpriteBatch.TextureUnit type)
        {
            Width = width;
            Height = height;
            Size = count;
            Type = type;

            GL_ID = GL.GenTexture();
            GL.ActiveTexture(TextureUnit.Texture0 + (int)Type);
            GL.BindTexture(TextureTarget.Texture2DArray, GL_ID);
            GL.TexStorage3D(TextureTarget3d.Texture2DArray, 0, SizedInternalFormat.Rgba8, Width, Height, Size);

            GL.TexParameter(TextureTarget.Texture2DArray, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat);
            GL.TexParameter(TextureTarget.Texture2DArray, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat);
            if (smooth)
            {
                GL.TexParameter(TextureTarget.Texture2DArray, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2DArray, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear);
            }
            else
            {
                GL.TexParameter(TextureTarget.Texture2DArray, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Nearest);
                GL.TexParameter(TextureTarget.Texture2DArray, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Nearest);
            }
        }

        public Sprite UploadTexture(Bitmap bmp, int ux, int uy)
        {
            if (Counter == Size) return default;
            GL.ActiveTexture(TextureUnit.Texture0 + (int)Type);
            GL.BindTexture(TextureTarget.Texture2DArray, GL_ID);

            Counter += 1;

            BitmapData data = bmp.LockBits(new Rectangle(0, 0, bmp.Width, bmp.Height), ImageLockMode.ReadOnly, System.Drawing.Imaging.PixelFormat.Format32bppArgb);
            GL.TexSubImage3D(TextureTarget.Texture2DArray, 0, 0, 0, Counter, data.Width, data.Height, 1, OpenTK.Graphics.OpenGL.PixelFormat.Bgra, PixelType.UnsignedByte, data.Scan0);
            bmp.UnlockBits(data);

            return new Sprite(Counter, bmp.Width, bmp.Height, ux, uy, Type);
        }

        public static implicit operator int(TextureArray o)
        {
            return o.GL_ID;
        }
    }
}
