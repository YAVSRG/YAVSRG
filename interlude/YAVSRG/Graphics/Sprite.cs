namespace Interlude.Graphics
{
    public struct Sprite
    {
        public int GL_Texture_ID;
        public int Width;
        public int Height;
        public int Columns;
        public int Rows;
        public int SourceWidth;
        public int SourceHeight;
        public int Offset_X;
        public int Offset_Y;

        public Sprite(int id, int width, int height, int columns, int rows, int sw, int sh, int offsetx, int offsety)
        {
            GL_Texture_ID = id;
            Width = width;
            Height = height;
            Columns = columns;
            Rows = rows;
            SourceWidth = sw;
            SourceHeight = sh;
            Offset_X = offsetx;
            Offset_Y = offsety;
        }

        public Sprite(int id, int width, int height, int columns, int rows) : this(id, width, height, columns, rows, width, height, 0, 0)
        {

        }

        public static Sprite Default => Game.Options.Themes.GetTexture("");
    }
}
