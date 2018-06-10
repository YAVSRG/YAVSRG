using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
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

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (ScreenUtils.MouseOver(left, top, right, bottom))
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
