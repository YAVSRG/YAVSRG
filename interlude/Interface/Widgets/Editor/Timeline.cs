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
        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(127, 0, 0, 0));
            float nowPos;
            foreach (Charts.YAVSRG.BPMPoint b in Game.CurrentChart.Timing.BPM.Points)
            {
                nowPos = (float)(bounds.Left + b.Offset / Game.Audio.Duration * bounds.Width);
                //SpriteBatch.DrawRect(new Rect(nowPos - 1, bounds.Top, nowPos + 1, bounds.Top + 25), (b.InheritsFrom != b.Offset) ? Color.Green : Color.Red);
            }
            nowPos = bounds.Left + bounds.Width * Game.Audio.NowPercentage();
            SpriteBatch.DrawRect(new Rect(nowPos - 2, bounds.Top, nowPos + 2, bounds.Bottom), Color.White);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.MouseOver(bounds))
            {
                if (Input.MousePress(OpenTK.Input.MouseButton.Left))
                {
                    double percent = (Input.MouseX - bounds.Left) / bounds.Width;
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
