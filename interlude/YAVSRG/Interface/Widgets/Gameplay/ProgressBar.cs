using System.Drawing;
using Interlude.Gameplay;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Gameplay
{
    public class ProgressBar : GameplayWidget
    {
        bool background;

        public ProgressBar(ScoreTracker scoreTracker, Options.WidgetPosition pos) : base(scoreTracker, pos)
        {
            background = pos.GetValue("Background", true);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float progress = ((float)Game.Audio.Now() - Game.CurrentChart.Notes.Points[0].Offset) / Game.CurrentChart.GetDuration();
            if (background) SpriteBatch.DrawRect(bounds, Color.FromArgb(((Color)scoreTracker.WidgetColor).A, Game.Screens.DarkColor));
            SpriteBatch.DrawRect(bounds.SliceLeft(bounds.Width * progress), Color.FromArgb(((Color)scoreTracker.WidgetColor).A, Game.Screens.HighlightColor));
        }
    }
}
