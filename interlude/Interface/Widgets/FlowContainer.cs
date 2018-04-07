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

        public FlowContainer(float padx, float pady) : base() //variable to control vertical, horizontal or both
        {
            padX = padx;
            padY = pady;
            //all widgets must be anchored to top left
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float x = left + padX;
            float y = top + padY;
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
                    w.B.Target(x + w.Width, y + w.Height);
                    w.A.Target(x, y);
                    //x += padX + w.Width; //only vertical flow
                    y += padY + w.Height;
                }
            }
            B.Target(x, y);
        }
    }
}
