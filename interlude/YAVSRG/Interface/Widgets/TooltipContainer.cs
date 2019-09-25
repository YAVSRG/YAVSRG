using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Widgets
{
    public class TooltipContainer : Widget
    {
        string text;

        public TooltipContainer(Widget w, string text) : base()
        {
            AddChild(w);
            this.text = text;
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.MouseOver(bounds)) Game.Screens.SetTooltip(text, "");
        }
    }
}
