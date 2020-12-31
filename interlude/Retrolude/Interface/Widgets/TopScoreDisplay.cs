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
            AddChild(scores.Reposition(10, 0, 10, 0, -210, 1, -10, 1));
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
                    scores.AddChild(new TopScoreCard(si, l, false).Reposition(0, 0, 0, 0, 0, 1, 80, 0));
                    l = !l;
                }
                return true;
            }, (b) => { }, "TopScores", false);
        }
    }
}
