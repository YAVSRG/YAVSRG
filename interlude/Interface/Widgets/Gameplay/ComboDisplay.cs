using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    public class ComboDisplay : GameplayWidget
    {
        AnimationSlider size;

        public ComboDisplay(ScoreTracker st) : base(st)
        {
            size = new AnimationSlider(40);
            st.OnHit += (x,y,z) =>
            {
                size.Val = 60;
                if (st.Scoring.Combo == 0)
                {
                    size.Val = 80;
                }
            };
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float s = Math.Min(50, scoreTracker.Scoring.Combo * 0.05f) + size;
            SpriteBatch.Font1.DrawCentredText(scoreTracker.Scoring.Combo.ToString(), s, (left + right) / 2, top - s / 2, scoreTracker.WidgetColor);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            size.Update();
        }
    }
}
