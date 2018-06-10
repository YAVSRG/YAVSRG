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

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Font1.DrawCentredText(title, 30f, (right + left) * 0.5f, top + 10f, Game.Options.Theme.MenuFont);
        }
    }
}
