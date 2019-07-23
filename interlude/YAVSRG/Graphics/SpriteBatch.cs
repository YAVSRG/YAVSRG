using System;
using OpenTK;
using OpenTK.Graphics.OpenGL;
using System.Drawing;
using Interlude.Interface;
using Interlude.IO;

namespace Interlude.Graphics
{
    class SpriteBatch
    {
        public enum StencilMode
        {
            Disable,
            Create,
            Draw
        }

        static int StencilDepth = 0;

        static double[] mat = new[] {
                1.0, 0.0, 0.0, 0.0,
                0.0, 0.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0,
            };

        public static SpriteFont Font1;
        public static SpriteFont Font2;

        public static void Draw(RenderTarget target, float depth = 0)
        {
            GL.Enable(EnableCap.Texture2D);
            GL.BindTexture(TextureTarget.Texture2D, target.Texture.ID);
            GL.Begin(PrimitiveType.Quads);

            GL.Color4(target.Color1);
            GL.TexCoord2(target.Texcoord1);
            GL.Vertex3(target.Coord1.X, target.Coord1.Y, depth);

            GL.Color4(target.Color2);
            GL.TexCoord2(target.Texcoord2);
            GL.Vertex3(target.Coord2.X, target.Coord2.Y, depth);

            GL.Color4(target.Color3);
            GL.TexCoord2(target.Texcoord3);
            GL.Vertex3(target.Coord3.X, target.Coord3.Y, depth);

            GL.Color4(target.Color4);
            GL.TexCoord2(target.Texcoord4);
            GL.Vertex3(target.Coord4.X, target.Coord4.Y, depth);

            GL.End();
            GL.Disable(EnableCap.Texture2D);
        }

        public static void Draw(string texture, Plane pos, int ux, int uy)
        {
            GL.Enable(EnableCap.Texture2D);
            Sprite s = Game.Options.Themes.GetTexture(texture);
            GL.BindTexture(TextureTarget.Texture2D, s.ID);
            GL.Begin(PrimitiveType.Quads);

            float x = 1f / s.UV_X;
            float y = 1f / s.UV_Y;

            GL.Color4(Color.White);

            GL.TexCoord2(x * ux, y * uy);
            GL.Vertex3(pos.P1);
            
            GL.TexCoord2(x + x * ux, y * uy);
            GL.Vertex3(pos.P2);

            GL.TexCoord2(x + x * ux, y + y * uy);
            GL.Vertex3(pos.P3);

            GL.TexCoord2(x * ux, y + y * uy);
            GL.Vertex3(pos.P4);

            GL.End();
            GL.Disable(EnableCap.Texture2D);
        }

        public static void DrawLine(Vector2 a, Vector2 b, Color col1, Color col2, float thickness)
        {
            GL.LineWidth(thickness);
            GL.Begin(PrimitiveType.Lines);
            GL.Color4(col1);
            GL.Vertex2(a);
            GL.Color4(col2);
            GL.Vertex2(b);
            GL.End();
        }

        public static RenderTarget Tiling(Sprite texture, Rect bounds, float offsetX = 0, float offsetY = 0, float scaleX = 1, float scaleY = 1, Color col1 = default, Color col2 = default, Color col3 = default, Color col4 = default)
        {
            float l = offsetX + bounds.Left / scaleX;
            float t = offsetY + bounds.Top / scaleY;
            float r = offsetX + bounds.Right / scaleX;
            float b = offsetY + bounds.Bottom / scaleY;

            return new RenderTarget(texture, bounds, col1, col2, col3, col4, new Vector2(l, t), new Vector2(r, t), new Vector2(r, b), new Vector2(l, b));
        }

        public static RenderTarget Tiling(Sprite texture, Rect bounds, float offsetX = 0, float offsetY = 0, float scaleX = 1, float scaleY = 1, Color col = default)
        {
            return Tiling(texture, bounds, offsetX, offsetY, scaleX, scaleY, col, col, col, col);
        }

        public static void DrawAlignedTexture(string texture, float x, float y, float scaleX, float scaleY, float alignX, float alignY, Color color)
        {
            Sprite s = Game.Options.Themes.GetTexture(texture);
            Draw(new RenderTarget(s, new Rect(x + s.Width * alignX * scaleX, y + s.Height * alignY * scaleY, x + s.Width * (alignX + 1) * scaleX, y + s.Height * (alignY + 1) * scaleY), color));
        }

        public static void DrawTilingTexture(string texture, Rect bounds, float scale, float x, float y, Color color)
        {
            DrawTilingTexture(Game.Options.Themes.GetTexture(texture), bounds, scale, scale, x, y, color, color, color, color);
        }

        public static void DrawTilingTexture(Sprite texture, Rect bounds, float scaleX, float scaleY, float x, float y, Color col1, Color col2, Color col3, Color col4)
        {
            Draw(Tiling(texture, bounds, x, y, scaleX, scaleY, col1, col2, col3, col4));
        }

        public static void DrawRect(Rect bounds, Color color)
        {
            Draw(new RenderTarget(default(Sprite), bounds, color, 0, 0));
        }

        public static void EnableTransform(bool upscroll)
        {
            int i = upscroll ? -1 : 1;
            mat[5] = -i;
            mat[4] = 0;
            float position = ScreenUtils.ScreenHeight * Game.Options.Profile.PerspectiveTilt * i;
            float height = (float)-Math.Pow(Math.Pow(-ScreenUtils.ScreenHeight, 2) - Math.Pow(position, 2), 0.5);
            Matrix4 m = Matrix4.LookAt(0, position, height, 0, 0, ScreenUtils.ScreenHeight, 0, -1, 0);
            GL.MatrixMode(MatrixMode.Modelview);
            GL.PushMatrix();
            GL.MultMatrix(ref m);
        }

        public static void Enable3D()
        {
            Matrix4 projection = Matrix4.CreatePerspectiveFieldOfView((float)Math.PI / 2f, (float)ScreenUtils.ScreenWidth / ScreenUtils.ScreenHeight, 1f, ScreenUtils.ScreenHeight * 2);
            GL.MatrixMode(MatrixMode.Projection);
            GL.PushMatrix();
            GL.LoadMatrix(ref projection);
        }

        public static void Disable3D()
        {
            GL.MatrixMode(MatrixMode.Projection);
            GL.PopMatrix();
        }

        public static void ParallelogramTransform(float amount, float center)
        {
            mat[4] = -amount;
            mat[5] = 1;
            GL.MatrixMode(MatrixMode.Modelview);
            GL.PushMatrix();
            GL.Translate(0, center, 0);
            GL.MultMatrix(mat);
            GL.Translate(0, -center, 0);
        }

        public static void DisableTransform()
        {
            GL.MatrixMode(MatrixMode.Modelview);
            GL.PopMatrix();
        }

        public static void AlphaTest(bool value) //works with stencil
        {
            if (value)
            {
                GL.Enable(EnableCap.AlphaTest);
                GL.AlphaFunc(AlphaFunction.Greater, 0);
            }
            else
            {
                GL.Disable(EnableCap.AlphaTest);
            }
        }

        public static void Stencil(StencilMode m)
        {
            if (m == StencilMode.Create)
            {
                if (StencilDepth == 0)
                {
                    GL.Enable(EnableCap.StencilTest);
                    GL.ClearStencil(0x00);
                    GL.Clear(ClearBufferMask.StencilBufferBit);
                }
                GL.StencilFunc(StencilFunction.Equal, StencilDepth, 0xFF);
                GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Incr);
                StencilDepth += 1;
            }
            else if (m == StencilMode.Draw)
            {
                GL.StencilFunc(StencilFunction.Equal, StencilDepth, 0xFF);
                GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Keep);
            }
            else if (m == StencilMode.Disable)
            {
                StencilDepth -= 1;
                if (StencilDepth == 0)
                {
                    GL.Clear(ClearBufferMask.StencilBufferBit);
                    GL.Disable(EnableCap.StencilTest);
                }
                else
                {
                    GL.StencilFunc(StencilFunction.Lequal, StencilDepth, 0xFF);
                }
            }
        }

        /*
        public static FBO WaterTest(FBO input)
        {
            //input should already be unbound
            GL.ActiveTexture(TextureUnit.Texture1);
            GL.BindTexture(TextureTarget.Texture2D, ((Sprite)input).ID);
            GL.ActiveTexture(TextureUnit.Texture0);
            int sampler;
            GL.GenSamplers(1, out sampler);
            GL.BindSampler(((Sprite)input).ID, sampler);
            FBO output = FBO.FromPool();
            GL.UseProgram(WaterShader.Program);
            GL.Uniform1(GL.GetUniformLocation(WaterShader.Program, "tex"), sampler);
            GL.Uniform1(GL.GetUniformLocation(WaterShader.Program, "time"), shader);
            shader += 0.4f;
            DrawRect(ScreenUtils.Bounds, Color.White);
            output.Unbind();
            input.Dispose();
            GL.DeleteSampler(sampler);
            return output;
        }*/

        public static void Begin(int width, int height)
        {
            GL.MatrixMode(MatrixMode.Projection);
            GL.LoadIdentity();
            GL.Ortho(-width * 0.5f, width * 0.5f, height * 0.5f, -height * 0.5f, -height, height);
        }

        public static void End()
        {
            GL.Finish();
            GL.Flush();
        }

        public static void Init()
        {
            GL.Enable(EnableCap.Blend);
            GL.Arb.BlendFuncSeparate(0, BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha, BlendingFactor.One, BlendingFactor.OneMinusSrcAlpha);
            GL.ClearColor(0, 0, 0, 0);
            GL.ClearStencil(0x00);

            Font1 = new SpriteFont(60, Game.Options.Theme.Font1);
            Font2 = new SpriteFont(60, Game.Options.Theme.Font2);

            FBO.InitBuffers();
        }
    }
}
