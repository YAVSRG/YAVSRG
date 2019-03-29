using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Gameplay;

namespace Interlude.Interface.Widgets
{
    public class TopScoreDisplay : FrameContainer
    {
        FlowContainer scores;
        bool Technical;
        Utilities.TaskManager.NamedTask Task;

        public TopScoreDisplay(bool tech)
        {
            Technical = tech;
            VerticalFade = 0;
            scores = new FlowContainer();
            AddChild(scores.PositionTopLeft(10, 10, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(210, 10, AnchorType.MAX, AnchorType.MAX));
        }

        public void Refresh(int keymode)
        {
            Task?.Cancel();
            scores.Clear();
            Task = Game.Tasks.AddTask((Output) =>
            {
                bool l = false;
                foreach (ScoreInfoProvider si in Technical ? Game.Options.Profile.Stats.GetTechnicalTop(keymode) : Game.Options.Profile.Stats.GetPhysicalTop(keymode))
                {
                    scores.AddChild(new TopScoreCard(si, l, false).PositionBottomRight(0, 80, AnchorType.MAX, AnchorType.MIN));
                    l = !l;
                }
                return true;
            }, (b) => { }, "TopScores", false);
        }
    }
}
