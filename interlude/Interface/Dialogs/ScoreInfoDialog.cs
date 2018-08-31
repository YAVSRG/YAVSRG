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
            A.Target(300, 200);
            B.Target(300, 200);
            Animation.Add(slide = new AnimationSlider(0) { Target = 1 });

            scoring = ScoreSystem.GetScoreSystem(ScoreType.Default);
            data = ScoreTracker.StringToHitData(score.hitdata, score.keycount);
            scoring.ProcessScore(data);
            acc = Utils.RoundNumber(scoring.Accuracy()) + "%";
            rating = Charts.DifficultyRating.PlayerRating.GetRating(new Charts.DifficultyRating.RatingReport(Game.Gameplay.GetModifiedChart(score.mods), score.rate, score.playstyle), data);
            mods = Game.Gameplay.GetModString(score.mods, score.rate, score.playstyle);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            int a = (int)(slide * 127);
            SpriteBatch.DrawRect(left, top, right, bottom, System.Drawing.Color.FromArgb(a, 0, 0, 0));
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            Game.Screens.DrawChartBackground(left, top, right, bottom, Game.Screens.DarkColor, 1f);
            SpriteBatch.DrawFrame(left, top, right, bottom, 30f, System.Drawing.Color.White);

            ScreenUtils.DrawGraph(left + 20, bottom - 200, right - 20, bottom - 20, scoring, data);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (!ScreenUtils.MouseOver(left,top,right,bottom) && Input.MouseClick(OpenTK.Input.MouseButton.Left))
            {
                Close("");
            }
        }
    }
}
