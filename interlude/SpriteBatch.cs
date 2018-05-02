using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK;
using OpenTK.Graphics.OpenGL;
using System.Drawing;
using YAVSRG.Interface;

namespace YAVSRG
{
    class SpriteBatch
    {/*
        struct Vertex
        {
            public static int SizeInBytes { get { return Vector2.SizeInBytes * 2 + Vector4.SizeInBytes; } }

            public Vector2 position;
            public Vector2 texCoord;
            public Vector4 color;
            public Vertex(Vector2 position, Vector2 texCoord, Color color)
            {
                this.position = position;
                this.texCoord = texCoord;
                this.color = new Vector4(color.R / 255f, color.G / 255f, color.B / 255f, color.A / 255f);
            }
        }*/
        
        public static SpriteFont Font1;
        public static SpriteFont Font2;

        static int VBO;

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, Color color, int rotation = 0)
        {
            Draw(texture, left, top, right, bottom, new Vector2[] { new Vector2(0, 0), new Vector2(1, 0), new Vector2(1, 1), new Vector2(0, 1) }, color, rotation);
        }

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, Color color, int ux, int uy, int rotation = 0)
        {
            Vector2[] coords = new[]
            {
                new Vector2(left,top),
                new Vector2(right,top),
                new Vector2(right,bottom),
                new Vector2(left,bottom)
            };
            Draw(texture, coords, color, ux, uy, rotation);
        }

        public static void Draw(Sprite texture, Vector2[] coords, Color color, int ux, int uy, int rotation = 0)
        {
            float x = 1f / texture.UV_X;
            float y = 1f / texture.UV_Y;
            Vector2[] texcoords = new[]
            {
                new Vector2(x * ux,y * uy),
                new Vector2(x + x*ux,y * uy),
                new Vector2(x + x*ux, y + y*uy),
                new Vector2(x*ux, y + y*uy)
            };
            Draw(texture, coords, texcoords, new Color[] { color, color, color, color }, rotation);
        }

        public static void Draw(Sprite texture, Vector2[] coords, Vector2[] texcoords, Color[] color, int rotation)
        {
            GL.Enable(EnableCap.Texture2D);
            GL.BindTexture(TextureTarget.Texture2D, texture.ID);
            GL.Begin(PrimitiveType.Quads);
            for (int i = 0; i < 4; i++)
            {
                GL.Color4(color[i]);
                GL.TexCoord2(texcoords[(i + rotation) % 4]);
                GL.Vertex2(coords[i]);
            }
            GL.End();
            GL.Disable(EnableCap.Texture2D);
        }

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, Vector2[] texcoords, Color color, int rotation = 0)
        {
            GL.Enable(EnableCap.Texture2D);
            Vector2[] coords = new[]
            {
                new Vector2(left,top),
                new Vector2(right,top),
                new Vector2(right,bottom),
                new Vector2(left,bottom)
            };
            Draw(texture, coords, texcoords, new[] { color, color, color, color }, rotation);
        }

        public static void DrawTilingTexture(Sprite texture, float left, float top, float right, float bottom, float scale, float x, float y, Color color)
        {
            RectangleF uv = new RectangleF(x+left/scale,y+top/scale,(right-left)/scale,(bottom-top)/scale);
            Draw(texture, left, top, right, bottom, VecArray(uv), color);
        }

        public static void DrawFrame(Sprite texture, float left, float top, float right, float bottom, float scale, Color color)
        {
            //corners
            Draw(texture, left, top, left + scale, top + scale, color, 0, 0);
            Draw(texture, right - scale, top, right, top + scale, color, 2, 0);
            Draw(texture, left, bottom - scale, left + scale, bottom, color, 0, 2);
            Draw(texture, right - scale, bottom - scale, right, bottom, color, 2, 2);
            //edges
            Draw(texture, left + scale, top, right - scale, top + scale, color, 1, 0);
            Draw(texture, left, top + scale, left + scale, bottom - scale, color, 0, 1);
            Draw(texture, right - scale, top + scale, right, bottom - scale, color, 2, 1);
            Draw(texture, left + scale, bottom - scale, right - scale, bottom, color, 1, 2);
        }

        public static void DrawRect(float left, float top, float right, float bottom, Color color)
        {
            GL.Begin(PrimitiveType.Quads);
            GL.Color4(color);

            GL.Vertex2(left, top);
            GL.Vertex2(right, top);
            GL.Vertex2(right, bottom);
            GL.Vertex2(left, bottom);

            GL.End();
        }

        public static void EnableTransform(bool upscroll)
        {
            double[] mat = new[] {
                1.0, 0.0, 0.0, 0.0,
                0.0, upscroll ? 1 : -1, 0.0, 0.0,
                0.0, 1.0, 1.0, 0.0,
                0.0, 0.0, 0.0, 1.0,
            };
            GL.MatrixMode(MatrixMode.Modelview);
            GL.PushMatrix();
            GL.Translate(0, (upscroll? -1 : 1) * ScreenUtils.ScreenHeight, 0);
            GL.MultMatrix(mat);
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

        public static void StencilMode(int m)
        {
            if (m == 1)
            {
                GL.StencilMask(0xFF);
                GL.Enable(EnableCap.StencilTest);
                GL.ClearStencil(0x00);
                GL.Clear(ClearBufferMask.StencilBufferBit);
                GL.StencilFunc(StencilFunction.Always, 1, 0xFF);
                GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Incr);
            }
            else if (m == 2)
            {
                GL.StencilMask(0x00);
                GL.StencilFunc(StencilFunction.Lequal, 1, 0xFF);
                GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Keep);
            }
            else if (m == 0)
            {
                GL.Clear(ClearBufferMask.StencilBufferBit);
                GL.Disable(EnableCap.StencilTest);
            }
        }

        public static void Begin(int width, int height)
        {
            GL.MatrixMode(MatrixMode.Projection);
            GL.LoadIdentity();
            GL.Ortho(-width * 0.5f, width * 0.5f, height * 0.5f, -height * 0.5f, -height, height);
        }

        public static void End()
        {
        }

        public static void Init()
        {
            GL.Enable(EnableCap.Blend);
            GL.BlendFunc(BlendingFactorSrc.SrcAlpha, BlendingFactorDest.OneMinusSrcAlpha);
            GL.ClearColor(0, 0, 0, 0);
            GL.ClearStencil(0x00);

            /*
            VBO = GL.GenBuffer();
            GL.BindBuffer(BufferTarget.ArrayBuffer, VBO);
            GL.EnableClientState(ArrayCap.VertexArray);
            GL.EnableClientState(ArrayCap.TextureCoordArray);
            GL.EnableClientState(ArrayCap.ColorArray);*/

            Font1 = new SpriteFont(60, "Akrobat Black");
            Font2 = new SpriteFont(60, "Akrobat");
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
