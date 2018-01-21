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
        PlayingChart playing;

        public ProgressBar(PlayingChart playing) : base()
        {
            this.playing = playing;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, right, bottom, Game.Options.Theme.Dark);
            float temp;
            float x = left;
            for(int i = 5; i >= 0; i--)
            {
                temp = playing.Scoring.Judgements[i] * (right - left) / playing.c.States.Count;
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
