using Interlude.Interface;
using Interlude.Interface.Widgets;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
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
