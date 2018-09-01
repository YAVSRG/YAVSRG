using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Options
{
    public class OptionsPanel : Widget
    {
        string title;

        public OptionsPanel(InfoBox ib, string title) : base()
        {
            this.title = title;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawCentredText(title, 30f, bounds.CenterX, bounds.Top + 10f, Game.Options.Theme.MenuFont);
        }
    }
}
