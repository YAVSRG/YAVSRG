using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface
{
    public struct Rect
    {
        public float Left, Top, Right, Bottom;

        public float Width
        {
            get { return Right - Left; }
        }

        public float Height
        {
            get { return Bottom - Top; }
        }

        public float CenterX
        {
            get { return (Right + Left) * 0.5f; }
        }

        public float CenterY
        {
            get { return (Bottom + Top) * 0.5f; }
        }

        public Rect ExpandX(float x)
        {
            return new Rect(Left - x, Top, Right + x, Bottom);
        }

        public Rect ExpandY(float y)
        {
            return new Rect(Left, Top - y, Right, Bottom + y);
        }

        public Rect Expand(float x, float y)
        {
            return new Rect(Left - x, Top - y, Right + x, Bottom + y);
        }

        public Rect SliceTop(float y)
        {
            return new Rect(Left, Top, Right, Top + y);
        }

        public Rect SliceBottom(float y)
        {
            return new Rect(Left, Bottom - y, Right, Bottom);
        }

        public Rect SliceLeft(float x)
        {
            return new Rect(Left, Top, Left + x, Bottom);
        }

        public Rect SliceRight(float x)
        {
            return new Rect(Right - x, Top, Right, Bottom);
        }

        public Rect FlipX()
        {
            return new Rect(Right, Top, Left, Bottom);
        }

        public Rect FlipY()
        {
            return new Rect(Left, Bottom, Right, Top);
        }

        public Rect(float left, float top, float right, float bottom)
        {
            Left = left;
            Top = top;
            Right = right;
            Bottom = bottom;
        }
    }
}
