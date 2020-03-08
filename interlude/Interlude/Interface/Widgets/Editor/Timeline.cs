using System.Drawing;
using Prelude.Gameplay.Charts.YAVSRG;
using Interlude.IO;
using Interlude.Graphics;
using System;

namespace Interlude.Interface.Widgets.Editor
{
    public class Timeline : Widget
    {
        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(127, 0, 0, 0));
            float nowPos;
            foreach (BPMPoint b in Game.CurrentChart.Timing.BPM.Points)
            {
                nowPos = (float)(bounds.Left + b.Offset / Game.Audio.Duration * bounds.Width);
                SpriteBatch.DrawRect(new Rect(nowPos - 1, bounds.Top, nowPos + 1, bounds.Top + 25), Color.Red);
            }
            nowPos = bounds.Left + bounds.Width * Game.Audio.NowPercentage();
            SpriteBatch.DrawRect(new Rect(nowPos - 2, bounds.Top, nowPos + 2, bounds.Bottom), Color.White);
            SpriteBatch.Font1.DrawCentredText(Utils.FormatTime((float)Game.Audio.Now()), 20f, Math.Max(bounds.Left + 25, nowPos), bounds.Top - 30, Color.White);
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
                if (Game.Audio.IsPaused)
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
