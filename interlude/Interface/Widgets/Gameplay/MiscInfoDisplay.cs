using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    class MiscInfoDisplay : GameplayWidget
    {
        Func<string> data;

        public MiscInfoDisplay(ScoreTracker scoreTracker, Func<string> data) : base(scoreTracker)
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
