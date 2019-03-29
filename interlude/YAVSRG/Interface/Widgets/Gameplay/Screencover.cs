using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Gameplay
{
    public class Screencover : GameplayWidget
    {
        bool flip;

        public Screencover(Interlude.Gameplay.ScoreTracker st, bool d) : base(st, new Options.WidgetPosition() { Enable = true })
        {
            flip = d;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (flip)
            {
                SpriteBatch.Draw("screencover", new Rect(bounds.Left, bounds.Top + Game.Options.Theme.ColumnWidth, bounds.Right, bounds.Bottom), System.Drawing.Color.White, 0, 1, depth: -Game.Options.Theme.NoteDepth);
                SpriteBatch.Draw("screencover", new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + Game.Options.Theme.ColumnWidth), System.Drawing.Color.White, 0, 0, depth: -Game.Options.Theme.NoteDepth);
            }
            else
            {
                SpriteBatch.Draw("screencover", new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Bottom - Game.Options.Theme.ColumnWidth), System.Drawing.Color.White, 0, 1, depth: -Game.Options.Theme.NoteDepth);
                SpriteBatch.Draw("screencover", new Rect(bounds.Left, bounds.Bottom - Game.Options.Theme.ColumnWidth, bounds.Right, bounds.Bottom), System.Drawing.Color.White, 0, 0, 2, depth: -Game.Options.Theme.NoteDepth);
            }
        }
    }
}
