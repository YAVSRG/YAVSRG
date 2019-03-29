using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Graphics.OpenGL;
using Interlude.Interface;

namespace Interlude.Graphics
{
    public class DrawableFBO : IDisposable
    {
        readonly int Texture_ID;
        readonly int FBO_ID;
        readonly Sprite Sprite;

        static List<int> FBO_STACK = new List<int>();
        static int[] FBO_POOL = new int[4];
        static int[] TEXTURE_POOL = new int[4];
        static int FBO_DEPTH = 0;

        public DrawableFBO()
        {
            // Generate the texture.
            if (FBO_POOL[FBO_DEPTH] == 0)
            {
                Texture_ID = GL.GenTexture();
                GL.BindTexture(TextureTarget.Texture2D, Texture_ID);
                GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0, PixelFormat.Rgba, PixelType.UnsignedByte, IntPtr.Zero);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.ClampToBorder);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.ClampToBorder);

                // Create a FBO and attach the texture.
                GL.Ext.GenFramebuffers(1, out FBO_ID);
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, FBO_ID);
                GL.RenderbufferStorage(RenderbufferTarget.RenderbufferExt, RenderbufferStorage.Depth24Stencil8, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2);
                //GL.Ext.FramebufferTexture2D(FramebufferTarget.FramebufferExt, FramebufferAttachment.DepthStencilAttachment, TextureTarget.Texture2D, Texture_ID, 0);
                GL.Ext.FramebufferTexture2D(FramebufferTarget.FramebufferExt, FramebufferAttachment.ColorAttachment0Ext, TextureTarget.Texture2D, Texture_ID, 0);

                TEXTURE_POOL[FBO_DEPTH] = Texture_ID;
                FBO_POOL[FBO_DEPTH] = FBO_ID;
            }
            else
            {
                Texture_ID = TEXTURE_POOL[FBO_DEPTH];
                GL.BindTexture(TextureTarget.Texture2D, Texture_ID);
                FBO_ID = FBO_POOL[FBO_DEPTH];
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, FBO_ID);
                GL.Clear(ClearBufferMask.ColorBufferBit);
            }

            Sprite = new Sprite(Texture_ID, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 1, 1);
            if (FBO_STACK.Count == 0)
            {
                GL.Ortho(-1, 1, 1, -1, -1, 1);
                GL.Viewport(0, 0, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2);
            }
            FBO_DEPTH++;
            FBO_STACK.Add(FBO_ID);
        }

        public void Unbind()
        {
            FBO_STACK.RemoveAt(FBO_STACK.Count - 1);
            if (FBO_STACK.Count == 0)
            {
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0);
                GL.Ortho(-1, 1, 1, -1, -1, 1);
                GL.Viewport(Game.Instance.ClientRectangle);
            }
            else
            {
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, FBO_STACK.Last());
            }
        }

        public void Dispose()
        {
            FBO_DEPTH--;
        }

        public static implicit operator Sprite(DrawableFBO fbo)
        {
            return fbo.Sprite;
        }

        public static void ClearPool()
        {
            for (int i = 0; i < 4; i++)
            {
                if (TEXTURE_POOL[i] != 0)
                    GL.DeleteTextures(1, ref TEXTURE_POOL[i]);
                TEXTURE_POOL[i] = 0;

                if (FBO_POOL[i] != 0)
                    GL.Ext.DeleteFramebuffers(1, ref FBO_POOL[i]);
                FBO_POOL[i] = 0;
            }
        }
    }
}
