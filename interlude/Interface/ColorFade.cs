using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface
{
    public class ColorFade : SlidingEffect
    {
        protected Color a;
        protected Color b;

        public ColorFade(Color a, Color b): base(0)
        {
            this.a = a;
            this.b = b;
        }

        public static implicit operator Color(ColorFade s)
        {
            float val1 = s.Val;
            float val2 = 1 - s.Val;
            return Color.FromArgb((int)(s.a.A * val2 + s.b.A * val1), (int)(s.a.R * val2 + s.b.R * val1), (int)(s.a.G * val2 + s.b.G * val1), (int)(s.a.B * val2 + s.b.B * val1));
        }

    }
}
