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

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Font1.DrawCentredTextToFill(data(), left, top, right, bottom, scoreTracker.WidgetColor);
        }
    }
}
