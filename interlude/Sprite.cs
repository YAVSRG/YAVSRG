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

        public Sprite(int id, int width, int height)
        {
            ID = id;
            Width = width;
            Height = height;
        }
    }
}
