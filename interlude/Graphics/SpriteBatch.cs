using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK;
using OpenTK.Graphics.OpenGL;
using System.Drawing;
using YAVSRG.Interface;
using YAVSRG.IO;

namespace YAVSRG.Graphics
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
        static float shader = 0f;
        static int calls = 0;

        public static SpriteFont Font1;
        public static SpriteFont Font2;

        public static Shader WaterShader;

        public static void Draw(Sprite sprite, Rect bounds, Color color, int rotation = 0)
        {
            Draw(sprite: sprite, bounds: bounds, texcoords: new Vector2[] { new Vector2(0, 0), new Vector2(1, 0), new Vector2(1, 1), new Vector2(0, 1) }, color: color, rotation: rotation);
        }

        public static void Draw(string texture = "", Rect bounds = default(Rect), Color color = default(Color), int ux = 0, int uy = 0, int rotation = 0, Sprite? sprite = null, Vector2[] coords = null, Vector2[] texcoords = null, Color[] colors = null, float depth = 0)
        {
            if (coords == null)
            {
                coords = new[] {
                    new Vector2(bounds.Left,bounds.Top),
                    new Vector2(bounds.Right,bounds.Top),
                    new Vector2(bounds.Right,bounds.Bottom),
                    new Vector2(bounds.Left,bounds.Bottom)
                };
            }
            if (colors == null)
            {
                colors = new[] { color, color, color, color };
            }
            if (sprite == null)
            {
                if (texture == "")
                {
                    Draw(coords, colors);
                    return;
                }
                //todo: have this stuff cached in theme data
                sprite = Content.GetTexture(texture);
            }
            if (texcoords == null)
            {
                float x = 1f / ((Sprite)sprite).UV_X;
                float y = 1f / ((Sprite)sprite).UV_Y;
                texcoords = new[] {
                    new Vector2(x * ux,y * uy),
                    new Vector2(x + x*ux,y * uy),
                    new Vector2(x + x*ux, y + y*uy),
                    new Vector2(x*ux, y + y*uy)
                };
            }
            Draw((Sprite)sprite, coords, texcoords, colors, rotation, depth);
        }

        public static void Draw(Vector2[] coords, Color[] color)
        {
            GL.Begin(PrimitiveType.Quads);
            for (int i = 0; i < 4; i++)
            {
                GL.Color4(color[i]);
                GL.Vertex2(coords[i]);
            }
            GL.End();
        }

        public static void Draw(Sprite texture, Vector2[] coords, Vector2[] texcoords, Color[] color, int rotation, float depth)
        {
            GL.Enable(EnableCap.Texture2D);
            GL.BindTexture(TextureTarget.Texture2D, texture.ID);
            GL.Begin(PrimitiveType.Quads);
            calls += 1;
            for (int i = 0; i < 4; i++)
            {
                GL.Color4(color[i]);
                GL.TexCoord2(texcoords[(i + rotation) % 4]);
                GL.Vertex3(coords[i].X, coords[i].Y, depth);
            }
            GL.End();
            GL.Disable(EnableCap.Texture2D);
        }

        public static void DrawTiling(string texture = "", Rect bounds = default(Rect), Color color = default(Color), float scaleX = 1f, float scaleY = 1f, float offsetX = 0, float offsetY = 0, int rotation = 0, Sprite? sprite = null, Vector2[] coords = null, Color[] colors = null, float depth = 0)
        {
            //vector2 coords not supported
            RectangleF uv = new RectangleF((offsetX + bounds.Left) / scaleX, (offsetY + bounds.Top) / scaleY, bounds.Width / scaleX, bounds.Height / scaleY);
            Draw(texture: texture, bounds: bounds, color: color, rotation: rotation, sprite: sprite, coords: coords, texcoords: VecArray(uv), colors: colors, depth: depth);
        }

        public static void DrawAlignedTexture(string texture, float x, float y, float scaleX, float scaleY, float alignX, float alignY, Color color)
        {
            Sprite s = Content.GetTexture(texture);
            Draw(texture: texture, bounds: new Rect(x + s.Width * alignX * scaleX, y + s.Height * alignY * scaleY, x + s.Width * (alignX + 1) * scaleX, y + s.Height * (alignY + 1) * scaleY), color: color);
        }

        public static void DrawTilingTexture(string texture, Rect bounds, float scale, float x, float y, Color color)
        {
            DrawTilingTexture(Content.GetTexture(texture), bounds, scale, scale, x, y, new Color[] { color, color, color, color });
        }

        public static void DrawTilingTexture(Sprite texture, Rect bounds, float scaleX, float scaleY, float x, float y, Color[] colors)
        {
            RectangleF uv = new RectangleF(x + bounds.Left / scaleX, y + bounds.Top / scaleY, bounds.Width / scaleX, bounds.Height / scaleY);
            Draw(sprite: texture, bounds: bounds, texcoords: VecArray(uv), colors: colors);
        }

        public static void DrawRect(Rect bounds, Color color)
        {
            GL.Begin(PrimitiveType.Quads);
            calls += 1;
            GL.Color4(color);

            GL.Vertex2(bounds.Left, bounds.Top);
            GL.Vertex2(bounds.Right, bounds.Top);
            GL.Vertex2(bounds.Right, bounds.Bottom);
            GL.Vertex2(bounds.Left, bounds.Bottom);

            GL.End();
        }

        public static void EnableTransform(bool upscroll)
        {
            int i = upscroll ? -1 : 1;
            double[] mat = new[] {
                1.0, 0.0, 0.0, 0.0,
                0.0, -i, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0,
            };
            float position = ScreenUtils.ScreenHeight * Game.Options.Profile.PerspectiveTilt * i;
            float height = (float)-Math.Pow(Math.Pow(-ScreenUtils.ScreenHeight, 2) - Math.Pow(position, 2), 0.5);
            Matrix4 m = Matrix4.LookAt(0, position, height, 0, 0, ScreenUtils.ScreenHeight, 0, -1, 0);
            GL.MatrixMode(MatrixMode.Modelview);
            GL.PushMatrix();
            GL.MultMatrix(ref m);
            GL.Translate(0, i * ScreenUtils.ScreenHeight, 0);
            GL.MultMatrix(mat);
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
            double[] mat = new[] {
                1.0, 0.0, 0.0, 0.0,
                -amount, 1.0, 0.0, 0.0,
                0.0, 0.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0,
            };
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

        public static DrawableFBO WaterTest(DrawableFBO input)
        {
            //input should already be unbound
            GL.ActiveTexture(TextureUnit.Texture1);
            GL.BindTexture(TextureTarget.Texture2D, ((Sprite)input).ID);
            GL.ActiveTexture(TextureUnit.Texture0);
            GL.GenSamplers(1, out int sampler);
            GL.BindSampler(((Sprite)input).ID, sampler);
            DrawableFBO output = new DrawableFBO();
            GL.UseProgram(WaterShader.Program);
            GL.Uniform1(GL.GetUniformLocation(WaterShader.Program, "tex"), sampler);
            GL.Uniform1(GL.GetUniformLocation(WaterShader.Program, "time"), shader);
            shader += 0.4f;
            DrawRect(ScreenUtils.Bounds, Color.White);
            output.Unbind();
            input.Dispose();
            GL.DeleteSampler(sampler);
            return output;
        }

        public static void Begin(int width, int height)
        {
            GL.MatrixMode(MatrixMode.Projection);
            GL.LoadIdentity();
            GL.Ortho(-width * 0.5f, width * 0.5f, height * 0.5f, -height * 0.5f, -height, height);
            calls = 0;
        }

        public static void End()
        {
            Console.WriteLine(calls);
        }

        public static void Init()
        {
            GL.Enable(EnableCap.Blend);
            GL.Arb.BlendFuncSeparate(0, BlendingFactor.SrcAlpha, BlendingFactor.OneMinusSrcAlpha, BlendingFactor.One, BlendingFactor.OneMinusSrcAlpha);
            GL.ClearColor(0, 0, 0, 0);
            GL.ClearStencil(0x00);

            Font1 = new SpriteFont(60, Game.Options.Theme.Font1);
            Font2 = new SpriteFont(60, Game.Options.Theme.Font2);

            WaterShader = new Shader(IO.ResourceGetter.GetShader("Vertex.vsh"), IO.ResourceGetter.GetShader("Water.fsh"));
        }

        public static Vector2[] VecArray(RectangleF rect)
        {
            return new[]
            {
                new Vector2(rect.Left,rect.Top),
                new Vector2(rect.Right,rect.Top),
                new Vector2(rect.Right,rect.Bottom),
                new Vector2(rect.Left,rect.Bottom)
            };
        }
    }
}
