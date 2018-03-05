using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Interface.Widgets
{
    public class ProgressBar : Widget
    {
        ScoreTracker playing;

        public ProgressBar(ScoreTracker playing) : base()
        {
            this.playing = playing;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, right, bottom, Game.Screens.DarkColor);
            float temp;
            float x = left;
            for(int i = 5; i >= 0; i--)
            {
                temp = playing.Scoring.Judgements[i] * (right - left) / playing.maxcombo;
                SpriteBatch.DrawRect(x, top, x + temp, bottom, Game.Options.Theme.JudgeColors[i]);
                x += temp;
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
        }
    }
}
