using OpenTK;
using System.Drawing;
using Interlude.Interface;

namespace Interlude.Graphics
{
    public struct RenderTarget
    {
        public Vector2 Coord1, Coord2, Coord3, Coord4;

        public Vector2 Texcoord1, Texcoord2, Texcoord3, Texcoord4;

        public Color Color1, Color2, Color3, Color4;

        public Sprite Texture;

        public RenderTarget(Sprite texture, Rect bounds, Color color, int ux = 0, int uy = 0)
        {
            Coord1 = new Vector2(bounds.Left, bounds.Top);
            Coord2 = new Vector2(bounds.Right, bounds.Top);
            Coord3 = new Vector2(bounds.Right, bounds.Bottom);
            Coord4 = new Vector2(bounds.Left, bounds.Bottom);

            float x = 1f / texture.UV_X;
            float y = 1f / texture.UV_Y;

            Texcoord1 = new Vector2(x * ux, y * uy);
            Texcoord2 = new Vector2(x + x * ux, y * uy);
            Texcoord3 = new Vector2(x + x * ux, y + y * uy);
            Texcoord4 = new Vector2(x * ux, y + y * uy);

            Color1 = Color2 = Color3 = Color4 = color;

            Texture = texture;
        }

        public RenderTarget(Sprite texture, Rect bounds, Color color, Vector2 uv1, Vector2 uv2, Vector2 uv3, Vector2 uv4)
        {
            Coord1 = new Vector2(bounds.Left, bounds.Top);
            Coord2 = new Vector2(bounds.Right, bounds.Top);
            Coord3 = new Vector2(bounds.Right, bounds.Bottom);
            Coord4 = new Vector2(bounds.Left, bounds.Bottom);

            Texcoord1 = uv1;
            Texcoord2 = uv2;
            Texcoord3 = uv3;
            Texcoord4 = uv4;

            Color1 = Color2 = Color3 = Color4 = color;

            Texture = texture;
        }

        public RenderTarget(Sprite texture, Rect bounds, Color col1, Color col2, Color col3, Color col4, Vector2 uv1, Vector2 uv2, Vector2 uv3, Vector2 uv4)
        {
            Coord1 = new Vector2(bounds.Left, bounds.Top);
            Coord2 = new Vector2(bounds.Right, bounds.Top);
            Coord3 = new Vector2(bounds.Right, bounds.Bottom);
            Coord4 = new Vector2(bounds.Left, bounds.Bottom);

            Texcoord1 = uv1;
            Texcoord2 = uv2;
            Texcoord3 = uv3;
            Texcoord4 = uv4;

            Color1 = col1;
            Color2 = col2;
            Color3 = col3;
            Color4 = col4;

            Texture = texture;
        }

        public RenderTarget(Sprite texture, Vector2 pos1, Vector2 pos2, Vector2 pos3, Vector2 pos4, Color col1, Color col2, Color col3, Color col4, Vector2 uv1, Vector2 uv2, Vector2 uv3, Vector2 uv4)
        {
            Coord1 = pos1;
            Coord2 = pos2;
            Coord3 = pos3;
            Coord4 = pos4;

            Texcoord1 = uv1;
            Texcoord2 = uv2;
            Texcoord3 = uv3;
            Texcoord4 = uv4;

            Color1 = col1;
            Color2 = col2;
            Color3 = col3;
            Color4 = col4;

            Texture = texture;
        }

        public RenderTarget(Vector2 pos1, Vector2 pos2, Vector2 pos3, Vector2 pos4, Color col)
        {
            Coord1 = pos1;
            Coord2 = pos2;
            Coord3 = pos3;
            Coord4 = pos4;

            Texcoord1 = Texcoord2= Texcoord3 = Texcoord4 = default(Vector2);

            Color1 = col;
            Color2 = col;
            Color3 = col;
            Color4 = col;

            Texture = default(Sprite);
        }

        public Vector2 GetTexCoord(int i)
        {
            switch (i)
            {
                case 3: return Texcoord4;
                case 2: return Texcoord3;
                case 1: return Texcoord2;
                case 0: default: return Texcoord1;
            }
        }

        /*
        public void SetTexCoord(int i, Vector2 val)
        {
            switch (i)
            {
                case 3: Texcoord4 = val; return;
                case 2: Texcoord3 = val; return;
                case 1: Texcoord2 = val; return;
                case 0: Texcoord1 = val; return;
                default: return;
            }
        }*/

        public RenderTarget Rotate(int r)
        {
            return new RenderTarget(Texture, Coord1, Coord2, Coord3, Coord4, Color1, Color2, Color3, Color4, GetTexCoord(r % 4), GetTexCoord((r + 1) % 4), GetTexCoord((r + 2) % 4), GetTexCoord((r + 3) % 4));
        }
    }
}
