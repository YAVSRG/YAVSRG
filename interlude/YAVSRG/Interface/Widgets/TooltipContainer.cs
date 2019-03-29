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
        bool hover = false;
        InfoBox target;

        public TooltipContainer(Widget w, string text, InfoBox ib) : base()
        {
            AddChild(w);
            this.text = text;
            target = ib;
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.MouseOver(bounds))
            {
                if (!hover)
                {
                    hover = true;
                    target.SetText(text);
                }
            }
            else
            {
                if (hover)
                {
                    hover = false;
                    target.SetText("");
                }
            }
        }
    }
}
