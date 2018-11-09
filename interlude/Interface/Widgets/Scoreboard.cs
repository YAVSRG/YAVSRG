using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Interface.Widgets
{
    public class Scoreboard : ScrollContainer
    {
        public Scoreboard() : base(10, 10, false)
        {

        }

        public void UseScoreList(List<Score> scores)
        {
            Children.Clear();
            scroll = 0;
            foreach (Score s in scores)
            {
                ScoreCard t = new ScoreCard(new ScoreInfoProvider(s,Game.CurrentChart));
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
