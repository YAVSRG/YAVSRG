using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG
{
    public struct Sprite
    {
        public int ID;
        public int Width;
        public int Height;
        public int UV_X;
        public int UV_Y;

        public Sprite(int id, int width, int height, int ux, int uy)
        {
            ID = id;
            Width = width;
            Height = height;
            UV_X = ux;
            UV_Y = uy;
        }
    }
}
