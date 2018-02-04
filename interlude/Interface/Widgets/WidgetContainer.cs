using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class WidgetContainer : Widget
    {
        protected List<Widget> Widgets;

        public WidgetContainer() : base()
        {
            Widgets = new List<Widget>();
        }
        
        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
                    w.Draw(left, top, right, bottom);
                }
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            foreach (Widget w in Widgets)
            {
                if (w.State > 0)
                {
                    w.Update(left, top, right, bottom);
                }
            }
        }
    }
}
