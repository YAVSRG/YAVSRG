using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class FlowContainer : Widget
    {
        float padX;
        float padY;
        bool horizontal;

        public FlowContainer(float padx, float pady, bool style) : base()
        {
            padX = padx;
            padY = pady;
            horizontal = style;
            //all widgets must be anchored to top left
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float x = padX;
            float y = padY;
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
                    w.B.Target(x + w.Width, y + w.Height);
                    w.A.Target(x, y);
                    if (horizontal)
                    {
                        x += padX + w.Width;
                    }
                    else
                    {
                        y += padY + w.Height;
                    }
                }
            }
            B.Target(A.AbsX+x, A.AbsY+y);
        }
    }
}
