using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    public class AccMeter : GameplayWidget
    {
        string type;
        public AccMeter(ScoreTracker st) : base(st)
        {
            type = st.Scoring.FormatAcc().Split(new[] { ' ' }, 2)[1];
            if (type == "(YAV)") type = "";
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float h = bounds.Height;
            SpriteBatch.Font1.DrawCentredTextToFill(Utils.RoundNumber(scoreTracker.Accuracy()) + "%", new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + h * 0.75f), scoreTracker.WidgetColor);
            SpriteBatch.Font2.DrawCentredTextToFill(type, new Rect(bounds.Left, bounds.Bottom - h * 0.4f, bounds.Right, bounds.Bottom), scoreTracker.WidgetColor);
        }
    }
}
