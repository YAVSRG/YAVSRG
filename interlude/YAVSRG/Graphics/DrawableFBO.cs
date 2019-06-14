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
     * using (var fbo = new DrawableFBO()) {
     *      //draw code as usual, however this draws onto the FBO instead of the screen
     *      fbo.Unbind();
     *      SpriteBatch.Draw(fbo, ...) //draw the FBO or a portion of it to somewhere on the screen
     * }
     * 
     * This lets me do masking/fading edges on things that would otherwise be complicated maths on the CPU
     * GPUs are also excessively good at moving large amounts of pixel data around, that's what they're designed for
     */
    public class DrawableFBO : IDisposable
    {
        readonly int Texture_ID;
        readonly int FBO_ID;
        readonly Sprite Sprite;

        static readonly int FBO_POOL_SIZE = 4;

        //Keeps track of what FBO we are using/which one to fall back to when this one is done with
        static List<int> FBO_STACK = new List<int>();
        //Array of OpenGL ids for the FBOs available
        static int[] FBO_POOL = new int[FBO_POOL_SIZE];
        //Array of OpenGL ids for the textures corresponding to the FBOs
        static int[] TEXTURE_POOL = new int[FBO_POOL_SIZE];
        //How many FBOs are being used within one another
        static int FBO_DEPTH = 0;

        //When the constructor is called the FBO is bound
        public DrawableFBO()
        {
            if (FBO_POOL[FBO_DEPTH] == 0) //if the FBO at this point hasnt been created, create it
            {
                Texture_ID = GL.GenTexture();
                GL.BindTexture(TextureTarget.Texture2D, Texture_ID);
                GL.TexImage2D(TextureTarget.Texture2D, 0, PixelInternalFormat.Rgba, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2, 0, PixelFormat.Rgba, PixelType.UnsignedByte, IntPtr.Zero);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMinFilter, (int)TextureMinFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureMagFilter, (int)TextureMagFilter.Linear);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapS, (int)TextureWrapMode.Repeat);
                GL.TexParameter(TextureTarget.Texture2D, TextureParameterName.TextureWrapT, (int)TextureWrapMode.Repeat);

                GL.Ext.GenFramebuffers(1, out FBO_ID);
                GL.Ext.BindFramebuffer(FramebufferTarget.FramebufferExt, FBO_ID);
                GL.RenderbufferStorage(RenderbufferTarget.RenderbufferExt, RenderbufferStorage.Depth24Stencil8, ScreenUtils.ScreenWidth * 2, ScreenUtils.ScreenHeight * 2);
                GL.Ext.FramebufferTexture2D(FramebufferTarget.FramebufferExt, FramebufferAttachment.ColorAttachment0Ext, TextureTarget.Texture2D, Texture_ID, 0);

                TEXTURE_POOL[FBO_DEPTH] = Texture_ID;
                FBO_POOL[FBO_DEPTH] = FBO_ID;
            }
            else
            {
                //if it already exists, just recycle it. all FBOs are the full screen size so they can be repurposed for anything
                //generating new FBOs every frame can be very expensive depending on the GPU so recycling is needed
                Texture_ID = TEXTURE_POOL[FBO_DEPTH];
                FBO_ID = FBO_POOL[FBO_DEPTH];
                GL.BindTexture(TextureTarget.Texture2D, Texture_ID);
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

        //Stops drawing to the FBO and returns to drawing to the previous buffer (either the previous FBO or the screen itself)
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

        //Must be called when you are done with an FBO
        public void Dispose()
        {
            FBO_DEPTH--;
        }

        //Lets this object be cast to a sprite implicitly so you can put it in draw calls exactly like a sprite
        public static implicit operator Sprite(DrawableFBO fbo)
        {
            return fbo.Sprite;
        }

        //Destroys all existing FBOs - used when the game is resized as all the FBOs need to be remade to the new resolution or they wont work
        public static void ClearPool()
        {
            for (int i = 0; i < FBO_POOL_SIZE; i++)
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
