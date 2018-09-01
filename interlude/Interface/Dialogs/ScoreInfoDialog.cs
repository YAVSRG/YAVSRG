using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;
using YAVSRG.Gameplay;

namespace YAVSRG.Interface.Dialogs
{
    public class ScoreInfoDialog : Dialog
    {
        AnimationSlider slide;

        ScoreTracker.HitData[] data;
        string acc;
        float rating;
        string mods;
        ScoreSystem scoring;

        public ScoreInfoDialog(Score score, Action<string> a) : base(a)
        {
            PositionTopLeft(ScreenUtils.ScreenWidth, 200, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(ScreenUtils.ScreenWidth, 200, AnchorType.MAX, AnchorType.MAX);
            Move(new Rect(200, 200, 200, 200));
            Animation.Add(slide = new AnimationSlider(0) { Target = 1 });

            scoring = ScoreSystem.GetScoreSystem(ScoreType.Default);
            data = ScoreTracker.StringToHitData(score.hitdata, score.keycount);
            scoring.ProcessScore(data);
            acc = Utils.RoundNumber(scoring.Accuracy()) + "%";
            rating = Charts.DifficultyRating.PlayerRating.GetRating(new Charts.DifficultyRating.RatingReport(Game.Gameplay.GetModifiedChart(score.mods), score.rate, score.playstyle), data);
            mods = Game.Gameplay.GetModString(score.mods, score.rate, score.playstyle);
        }

        public override void Draw(Rect bounds)
        {
            int a = (int)(slide * 127);
            SpriteBatch.DrawRect(bounds, System.Drawing.Color.FromArgb(a, 0, 0, 0));
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            Game.Screens.DrawChartBackground(bounds, Game.Screens.DarkColor, 1f);
            ScreenUtils.DrawFrame(bounds, 30f, System.Drawing.Color.White);

            ScreenUtils.DrawGraph(new Rect(bounds.Left + 20, bounds.Bottom - 200, bounds.Right - 20, bounds.Bottom - 20), scoring, data);
        }

        public override void Update(Rect bounds)
        {
            //todo: make base "fading" dialog that dims game to show you something
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (!ScreenUtils.MouseOver(bounds) && Input.MouseClick(OpenTK.Input.MouseButton.Left))
            {
                Close("");
            }
        }
    }
}
