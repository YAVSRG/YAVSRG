using System.Collections.Generic;
using Prelude.Gameplay;
using Interlude.Gameplay;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class Scoreboard : Widget
    {
        protected FlowContainer ScoreContainer;
        bool noScores;
        string scoreType = "";

        public Scoreboard()
        {
            ScoreContainer = new FlowContainer() { BackColor = () => Utils.ColorInterp(System.Drawing.Color.FromArgb(80, 80, 80), Game.Screens.DarkColor, 0.2f) };
            ScoreContainer.Reposition(0, 0, 0, 0, 0, 1, -50, 1);
            AddChild(ScoreContainer);
        }

        public void UseScoreList(List<Score> scores)
        {
            scoreType = Game.Options.Profile.GetScoreSystem(Game.Options.Profile.SelectedScoreSystem).Name;
            ScoreContainer.Clear();
            ScoreContainer.ScrollPosition = 0;
            noScores = true;
            foreach (Score s in scores)
            {
                noScores = false;
                ScoreCard t = new ScoreCard(new ScoreInfoProvider(s, Game.CurrentChart));
                ScoreContainer.AddChild(t);
            }
            ScoreContainer.Sort(ScoreCard.Compare);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (noScores) SpriteBatch.Font1.DrawTextToFill("No local scores", bounds, Game.Options.Theme.MenuFont, true, System.Drawing.Color.Black);
            ScreenUtils.DrawParallelogramWithBG(bounds.SliceBottom(50), 0, Game.Screens.BaseColor, Game.Screens.HighlightColor);
            SpriteBatch.Font1.DrawCentredTextToFill(scoreType, bounds.SliceBottom(50), Game.Options.Theme.MenuFont, true, System.Drawing.Color.Black);
        }
    }
}
