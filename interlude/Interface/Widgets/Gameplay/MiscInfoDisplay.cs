using System;
using YAVSRG.Gameplay;
using YAVSRG.Graphics;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    class MiscInfoDisplay : GameplayWidget
    {
        Func<string> data;

        public MiscInfoDisplay(ScoreTracker scoreTracker, Options.WidgetPosition pos, Func<string> data) : base(scoreTracker, pos)
        {
            this.data = data;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawCentredTextToFill(data(), bounds, scoreTracker.WidgetColor);
        }
    }
}
