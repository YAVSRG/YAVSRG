using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Graphics.OpenGL;
using Interlude.Interface;

namespace Interlude.Graphics
{
    /*
     * Frame Buffer Objects
     * What this is for the less graphics savvy:
     * This is like a texture you can draw directly on, and then right after that you can draw the texture onto the screen (or even another of these)
     * This lets you render part of the game onto a texture, and then draw the texture recolored or rotated or whatever you want onto the screen
     * This is also how shaders work - you draw onto a texture like this and then the shader changes how it is drawn to the screen after
     * 
     * Example usage:
     * using (var fbo = FBO.FromPool()) {
     *      //draw code as usual, however this draws onto the FBO instead of the screen
     *      fbo.Unbind();
     *      SpriteBatch.Draw(fbo, ...) //draw the FBO or a portion of it to somewhere on the screen
     * }
     * 
     * This lets me do masking/fading edges on things that would otherwise be complicated maths on the CPU
     * GPUs are also excessively good at moving large amounts of pixel data around, that's what they're designed for
     */
    public class FBO : IDisposable
    {
        //Stores the sprite bound to a texture id that can be used to draw this fbo to the screen
        readonly Sprite Sprite;
        //Stores the id of the fbo to be used in binding and unbinding methods
        readonly int GL_FBO_ID;

        //Stores the index of where this fbo was in the pool
        readonly int FBO_Index;

        static readonly int FBO_POOL_SIZE = 6;

        //Pooling arrays for FBOs. They are generated all in one go and never again (except screen resize) to save on GPU usage
        static int[] POOL_FBO_ID = new int[FBO_POOL_SIZE]; //stores GL ids for fbos
        static int[] POOL_TEXTURE_ID = new int[FBO_POOL_SIZE]; //stores GL ids for corresponding textures
        static bool[] POOL_IN_USE = new bool[FBO_POOL_SIZE]; //bool for if this fbo is in use or not (for pooling)

        //Stack of which fbos are used in what order, so when you Unbind() it automatically binds to the previous FBO
        static List<int> USAGE_STACK = new List<int>();

        public FBO(int gL_Tex_ID, int gl_FBO_ID, int index)
        {
            Sprite = new Sprite(gL_Tex_ID, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 1, 1);
            FBO_Index = index;
            GL_FBO_ID = gl_FBO_ID;
            POOL_IN_USE[FBO_Index] = true;
            Bind();
            GL.Clear(ClearBufferMask.ColorBufferBit);
        }

        //Binds to an FBO so you are now drawing to it instead of the screen
        public FBO Bind()
        {
            if (USAGE_STACK.Count == 0)
            {
                GL.Ortho(-1, 1, 1, -1, -1, 1);
                GL.Viewport(0, 0, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2);
            }
            GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, GL_FBO_ID);
            USAGE_STACK.Add(GL_FBO_ID);
            return this;
        }

        //Unbinds from an FBO so you are now drawing to the screen again/the previous FBO you were drawing to
        public FBO Unbind()
        {
            USAGE_STACK.RemoveAt(USAGE_STACK.Count - 1);
            if (USAGE_STACK.Count == 0)
            {
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0);
                GL.Ortho(-1, 1, 1, -1, -1, 1);
                GL.Viewport(Game.Instance.ClientRectangle);
            }
            else
            {
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, USAGE_STACK.Last());
            }
            return this;
        }

        //Marks an FBO as done with so it can be recycled
        public void Dispose()
        {
            POOL_IN_USE[FBO_Index] = false;
        }

        public static implicit operator Sprite(FBO fbo)
        {
            return fbo.Sprite;
        }

        public static FBO FromPool()
        {
            for (int i = 0; i < FBO_POOL_SIZE; i++)
            {
                if (!POOL_IN_USE[i])
                {
                    return new FBO(POOL_TEXTURE_ID[i], POOL_FBO_ID[i], i);
                }
            }
            throw new Exception("All FBOs in pool are in use. FBO_POOL_SIZE should be larger or FBOs are not being disposed of");
        }

        public static void InitBuffers()
        {
            for (int i = 0; i < FBO_POOL_SIZE; i++)
            {
                if (POOL_TEXTURE_ID[i] != 0)
                    GL.DeleteTextures(1, ref POOL_TEXTURE_ID[i]);
                POOL_TEXTURE_ID[i] = 0;

                if (POOL_FBO_ID[i] != 0)
                    GL.Ext.DeleteFramebuffers(1, ref POOL_FBO_ID[i]);
                POOL_FBO_ID[i] = 0;

                POOL_TEXTURE_ID[i] = GL.GenTexture();
                GL.BindTexture(TextureTarget.Texture2D, POOL_TEXTURE_ID[i]);
                GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0, PixelFormat.Rgba, PixelType.UnsignedByte, IntPtr.Zero);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat);

                GL.Ext.GenFramebuffers(1, out POOL_FBO_ID[i]);
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, POOL_FBO_ID[i]);
                GL.RenderbufferStorage(RenderbufferTarget.RenderbufferExt, RenderbufferStorage.Depth24Stencil8, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2);
                GL.Ext.FramebufferTexture2D(FramebufferTarget.FramebufferExt, FramebufferAttachment.ColorAttachment0Ext, TextureTarget.Texture2D, POOL_TEXTURE_ID[i], 0);
            }

            GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, 0);
        }
    }
}
