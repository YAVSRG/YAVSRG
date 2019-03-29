using Interlude.Gameplay;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Gameplay
{
    public class AccMeter : GameplayWidget
    {
        string type;
        public AccMeter(ScoreTracker scoreTracker, Options.WidgetPosition pos) : base(scoreTracker, pos)
        {
            type = scoreTracker.Scoring.FormatAcc().Split(new[] { ' ' }, 2)[1];
            if (type == "(SC)") type = "";
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float h = bounds.Height;
            SpriteBatch.Font1.DrawCentredTextToFill(Utils.RoundNumber(scoreTracker.Scoring.Accuracy()) + "%", new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + h * 0.75f), scoreTracker.WidgetColor);
            SpriteBatch.Font2.DrawCentredTextToFill(type, new Rect(bounds.Left, bounds.Bottom - h * 0.4f, bounds.Right, bounds.Bottom), scoreTracker.WidgetColor);
        }
    }
}
