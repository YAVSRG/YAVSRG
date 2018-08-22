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
            foreach (Score s in scores)
            {
                ScoreCard t = new ScoreCard(s);
                AddChild(t);
                t.A.MoveTarget(0, -100);
                t.B.MoveTarget(0, -100);
            }
            Widgets.Sort(ScoreCard.Compare);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (Widgets.Count == 0)
            {
                SpriteBatch.Font1.DrawTextToFill("No local scores", left, top, right, bottom, Game.Options.Theme.MenuFont);
            }
        }
    }
}
