using Interlude.Gameplay;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Gameplay
{
    public class HPMeter : GameplayWidget
    {
        bool Horizontal;

        public HPMeter(ScoreTracker scoreTracker, Options.WidgetPosition pos) : base(scoreTracker, pos)
        {
            Horizontal = pos.GetValue("Horizontal", true);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (Horizontal)
            {
                SpriteBatch.DrawRect(bounds.SliceLeft(bounds.Width * scoreTracker.HP.GetValue()), System.Drawing.Color.White);
            }
            else
            {
                SpriteBatch.DrawRect(bounds.SliceBottom(bounds.Height * scoreTracker.HP.GetValue()), System.Drawing.Color.White);
            }
        }
    }
}
