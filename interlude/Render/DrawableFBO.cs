using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Graphics.OpenGL;
using YAVSRG.Interface;

namespace YAVSRG
{
    public class DrawableFBO : IDisposable
    {
        int Texture_ID;
        int FBO_ID;
        readonly Sprite Sprite;

        static List<int> FBO_Stack = new List<int>();

        public DrawableFBO(Shader shader)
        {
            // Generate the texture.
            Texture_ID = GL.GenTexture();
            GL.BindTexture(TextureTarget.Texture2D, Texture_ID);
            GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0, PixelFormat.Rgba, PixelType.UnsignedByte, IntPtr.Zero);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Nearest);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.ClampToBorder);
            GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.ClampToBorder);

            // Create a FBO and attach the texture.
            GL.Ext.GenFramebuffers(1, out FBO_ID);
            GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, FBO_ID);
            GL.Ext.FramebufferTexture2D(FramebufferTarget.FramebufferExt,
                FramebufferAttachment.ColorAttachment0Ext, TextureTarget.Texture2D, Texture_ID, 0);

            Sprite = new Sprite(Texture_ID, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 1, 1);
            GL.Ortho(-1, 1, 1, -1, -1, 1);
            if (shader != null)
            {
                GL.UseProgram(shader.Program);
            }
            FBO_Stack.Add(FBO_ID);
        }

        public void Unbind()
        {
            FBO_Stack.RemoveAt(FBO_Stack.Count - 1);
            if (FBO_Stack.Count == 0)
            {
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0);
            }
            else
            {
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, FBO_Stack[FBO_Stack.Count - 1]);
            }
            GL.Ortho(-1, 1, 1, -1, -1, 1);
            GL.UseProgram(0);
        }

        public static implicit operator Sprite(DrawableFBO fbo)
        {
            return fbo.Sprite;
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
