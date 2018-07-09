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
            type = st.Scoring.FormatAcc().Split(' ')[1];
            if (type == "(YAV)") type = "";
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float h = bottom-top;
            SpriteBatch.Font1.DrawCentredTextToFill(Utils.RoundNumber(scoreTracker.Accuracy()) + "%", left, top, right, top + h * 0.75f, scoreTracker.WidgetColor);
            SpriteBatch.Font2.DrawCentredTextToFill(type, left, bottom - h * 0.4f, right, bottom, scoreTracker.WidgetColor);
        }
    }
}
