using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    class WidgetSwitcher : Widget
    {
        int Current;

        public void Switch(int x)
        {
            Widgets[Current].State = 0;
            Current = x;
            Widgets[x].State = 1;
        }

        public override void AddChild(Widget child)
        {
            base.AddChild(child);
            child.State = 0;
        }
    }
}
