using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    class WidgetSwitcher : Widget
    {
        List<Widget> Widgets;
        int Current;

        public WidgetSwitcher()
        {
            Widgets = new List<Widget>();
        }

        public void Add(Widget w)
        {
            Widgets.Add(w);
        }

        public void Switch(int x)
        {
            Current = x;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            Widgets[Current].Draw(left, top, right, bottom);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            Widgets[Current].Update(left, top, right, bottom);
        }
    }
}
