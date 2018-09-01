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
            Widgets.Clear();
            scroll = 0;
            foreach (Score s in scores)
            {
                ScoreCard t = new ScoreCard(s);
                AddChild(t);
            }
            Widgets.Sort(ScoreCard.Compare);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (Widgets.Count == 0)
            {
                SpriteBatch.Font1.DrawTextToFill("No local scores", bounds, Game.Options.Theme.MenuFont);
            }
        }
    }
}
