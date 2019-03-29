using System.Collections.Generic;
using Prelude.Gameplay;
using Interlude.Gameplay;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class Scoreboard : FlowContainer
    {
        public Scoreboard()
        {
            MarginX = MarginY = 10;
        }

        public void UseScoreList(List<Score> scores)
        {
            Children.Clear();
            ScrollPosition = 0;
            foreach (Score s in scores)
            {
                ScoreCard t = new ScoreCard(new ScoreInfoProvider(s, Game.CurrentChart));
                AddChild(t);
            }
            Children.Sort(ScoreCard.Compare);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (Children.Count == 0)
            {
                SpriteBatch.Font1.DrawTextToFill("No local scores", bounds, Game.Options.Theme.MenuFont, true, System.Drawing.Color.Black);
            }
        }
    }
}
