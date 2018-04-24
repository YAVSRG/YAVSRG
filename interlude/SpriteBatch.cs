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

        static Dictionary<char, Sprite> FontLookup;
        public static SpriteFont Font1;
        public static SpriteFont Font2;

        static int VBO;

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, Color color, int rotation = 0)
        {
            Draw(texture, left, top, right, bottom, new Rectangle(0, 0, 1, 1), color, rotation);
        }

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, Color color, int ux, int uy, int rotation = 0)
        {
            float x = 1f / texture.UV_X;
            float y = 1f / texture.UV_Y;
            RectangleF UV = new RectangleF(x * ux, y * uy, x, y);
            Draw(texture, left, top, right, bottom, UV, color, rotation);
        }

        public static void Draw(Sprite texture, float left, float top, float right, float bottom, RectangleF uv, Color color, int rotation = 0)
        {
            GL.Enable(EnableCap.Texture2D);
            Vector2[] texcoords = new[]
            {
                new Vector2(uv.Left,uv.Top),
                new Vector2(uv.Right,uv.Top),
                new Vector2(uv.Right,uv.Bottom),
                new Vector2(uv.Left,uv.Bottom)
            };

            GL.BindTexture(TextureTarget.Texture2D, texture.ID);
            GL.Begin(PrimitiveType.Quads);

            GL.Color4(color);

            GL.TexCoord2(texcoords[rotation % 4]);
            GL.Vertex2(left, top);
            GL.TexCoord2(texcoords[(1 + rotation) % 4]);
            GL.Vertex2(right, top);
            GL.TexCoord2(texcoords[(2 + rotation) % 4]);
            GL.Vertex2(right, bottom);
            GL.TexCoord2(texcoords[(3 + rotation) % 4]);
            GL.Vertex2(left, bottom);

            GL.End();
            GL.Disable(EnableCap.Texture2D);
        }

        public static void DrawTilingTexture(Sprite texture, float left, float top, float right, float bottom, float scale, float x, float y, Color color)
        {
            RectangleF uv = new RectangleF(x+left/scale,y+top/scale,(right-left)/scale,(bottom-top)/scale);
            Draw(texture, left, top, right, bottom, uv, color);
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
                GL.StencilFunc(StencilFunction.Always, 1, 0xFF);
                GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Incr);
            }
            else if (m == 2)
            {
                GL.StencilMask(0x00);
                GL.Enable(EnableCap.StencilTest);
                GL.StencilFunc(StencilFunction.Less, 1, 0xFF);
                GL.StencilOp(StencilOp.Keep, StencilOp.Keep, StencilOp.Replace);
            }
            else if (m == 0)
            {
                GL.ClearStencil(0x00);
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

            /*
            VBO = GL.GenBuffer();
            GL.BindBuffer(BufferTarget.ArrayBuffer, VBO);
            GL.EnableClientState(ArrayCap.VertexArray);
            GL.EnableClientState(ArrayCap.TextureCoordArray);
            GL.EnableClientState(ArrayCap.ColorArray);*/

            Font1 = new SpriteFont(60, "Akrobat Black");
            Font2 = new SpriteFont(60, "Akrobat");
        }
    }
}
