using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface.Widgets.Editor
{
    public class Timeline : Widget
    {
        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, right, bottom, Color.FromArgb(127, 0, 0, 0));
            float nowPos;
            foreach (Charts.YAVSRG.BPMPoint b in Game.CurrentChart.Timing.Points)
            {
                nowPos = (float)(left + b.Offset / Game.Audio.Duration * (right - left));
                SpriteBatch.DrawRect(nowPos - 1, top, nowPos + 1, top + 25, (b.InheritsFrom != b.Offset) ? Color.Green : Color.Red);
            }
            nowPos = left + (right - left) * Game.Audio.NowPercentage();
            SpriteBatch.DrawRect(nowPos - 2, top, nowPos + 2, bottom, Color.White);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (ScreenUtils.MouseOver(left, top, right, bottom))
            {
                if (Input.MousePress(OpenTK.Input.MouseButton.Left))
                {
                    double percent = (Input.MouseX - left) / (right - left);
                    Game.Audio.Seek(Game.Audio.Duration * percent);
                }
            }
            if (Input.KeyTap(OpenTK.Input.Key.Space))
            {
                if (Game.Audio.Paused)
                {
                    Game.Audio.Play();
                }
                else
                {
                    Game.Audio.Pause();
                }
            }
        }
    }
}
