using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Graphics.OpenGL;
using YAVSRG.Interface;

namespace YAVSRG
{
    public class DrawableFBO
    {
        int Texture_ID;
        int FBO_ID;
        public readonly Sprite Sprite;

        public DrawableFBO(int Width, int Height)
        {
            // Generate the texture.
            Texture_ID = GL.GenTexture();
            GL.BindTexture(TextureTarget.Texture2D, Texture_ID);
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, Width, Height, 0, PixelFormat.Rgba, PixelType.UnsignedByte, IntPtr.Zero);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Nearest);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.ClampToBorder);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.ClampToBorder);

            // Create a FBO and attach the texture.
            GL.Ext.GenFramebuffers(1, out FBO_ID);
            GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, FBO_ID);
            GL.Ext.FramebufferTexture2D(FramebufferTarget.FramebufferExt,
                FramebufferAttachment.ColorAttachment0Ext, TextureTarget.Texture2D, FBO_ID, 0);

            Sprite = new Sprite(Texture_ID, Width, Height, 1, 1);
        }

        public void Draw(Rect bounds)
        {
            GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0);
            SpriteBatch.Draw(Sprite, bounds, System.Drawing.Color.White);
        }

        public void Dispose()
        {
            if (Texture_ID != 0)
                GL.DeleteTextures(1, ref Texture_ID);

            if (FBO_ID != 0)
                GL.Ext.DeleteFramebuffers(1, ref FBO_ID);
        }
    }
}
