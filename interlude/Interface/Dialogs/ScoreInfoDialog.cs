using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;
using YAVSRG.Gameplay;

namespace YAVSRG.Interface.Dialogs
{
    public class ScoreInfoDialog : FadeDialog
    {

        ScoreTracker.HitData[] data;
        string acc;
        float rating;
        string mods;
        ScoreSystem scoring;

        public ScoreInfoDialog(Score score, Action<string> a) : base(a)
        {
            PositionTopLeft(-ScreenUtils.ScreenWidth * 2 + 200, 200, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(ScreenUtils.ScreenWidth * 2 + 200, 200, AnchorType.MAX, AnchorType.MAX);
            Move(new Rect(200, 200, 200, 200));

            scoring = ScoreSystem.GetScoreSystem(ScoreType.Default);
            data = ScoreTracker.StringToHitData(score.hitdata, score.keycount);
            scoring.ProcessScore(data);
            acc = Utils.RoundNumber(scoring.Accuracy()) + "%";
            var chart = Game.Gameplay.GetModifiedChart(score.mods);
            rating = Charts.DifficultyRating.PlayerRating.GetRating(new Charts.DifficultyRating.RatingReport(chart, score.rate, score.playstyle), data);
            mods = Game.Gameplay.GetModString(chart, score.rate, score.playstyle);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            Game.Screens.DrawChartBackground(bounds, Game.Screens.DarkColor, 1f);
            ScreenUtils.DrawFrame(bounds, 30f, System.Drawing.Color.White);

            ScreenUtils.DrawGraph(new Rect(bounds.Left + 20, bounds.Bottom - 200, bounds.Right - 20, bounds.Bottom - 20), scoring, data);
        }

        protected override void OnClosing()
        {
            base.OnClosing();
            Move(new Rect(-ScreenUtils.ScreenWidth * 2 + 200, 200, ScreenUtils.ScreenWidth * 2 + 200, 200));
        }
    }
}
