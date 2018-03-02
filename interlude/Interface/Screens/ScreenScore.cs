using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using static YAVSRG.Interface.ScreenUtils;
using System.Drawing;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Screens
{
    class ScreenScore : Screen
    {
        static string[] ranks = new[] { "ss", "s", "a", "b", "c", "f" };
        static Color[] rankColors = new[] { Color.Gold, Color.Orange, Color.Green, Color.Blue, Color.Purple, Color.Gray };
        private PlayingChart score;
        Sprite rank;
        int tier;
        int snapcount;
        ScoreSystem acc1, acc2;

        public ScreenScore(PlayingChart data)
        {
            score = data;
            snapcount = score.c.States.Count;
            score.Scoring.BestCombo = Math.Max(score.Scoring.Combo, score.Scoring.BestCombo); //if your biggest combo was until the end of the map, this catches it

            float acc = score.Accuracy();
            tier = 5;
            for (int i = 0; i < Game.Options.Profile.AccGradeThresholds.Length; i++) //custom grade boundaries
            {
                if (acc >= Game.Options.Profile.AccGradeThresholds[i])
                {
                    tier = i; break;
                }
            }
            rank = Content.LoadTextureFromAssets("rank-" + ranks[tier]);

            ChartDifficulty c = new ChartDifficulty(Game.CurrentChart);
            c.PositionTopLeft(520, 80, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(20, 80, AnchorType.MAX, AnchorType.MAX);
            AddChild(c);

            acc1 = ScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == ScoreType.Osu) ? ScoreType.Default : ScoreType.Osu);
            acc2 = ScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == ScoreType.Wife || Game.Options.Profile.ScoreSystem == ScoreType.DP) ? ScoreType.Default : ScoreType.Wife);
            acc1.ProcessScore(score.hitdata);
            acc2.ProcessScore(score.hitdata);
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            toolbar.hide = false;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            //SpriteBatch.DrawCentredText(ranks[tier], 200f, 0, -320, rankColors[tier]);
            SpriteBatch.Draw(rank, -90, -280, 90, -80, Color.White);
            //you'll just have to change to the chart before showing the score screen
            //SpriteBatch.DrawCentredText(ChartLoader.SelectedChart.header.title + " [" + score.c.DifficultyName + "]", 30f, 0, -Height + 150, Color.White);
            //SpriteBatch.DrawCentredText(Utils.RoundNumber(Game.Options.Profile.Rate)+"x rate", 20f, 0, -Height + 200, Color.White);
            //SpriteBatch.DrawCentredTextToFill(ChartLoader.SelectedPack.title, -Width+300, -Height + 80, Width-300, -Height + 130, Color.White);

            SpriteBatch.DrawCentredText(score.Scoring.FormatAcc(), 50, 0, -50, Color.White);
            SpriteBatch.DrawCentredText(acc1.FormatAcc(), 30, -250, 50, Color.White);
            SpriteBatch.DrawCentredText(acc2.FormatAcc(), 30, 250, 50, Color.White);
            for (int i = 0; i < 6; i++)
            {
                SpriteBatch.DrawRect(-Width + 50, 100 + i * 40, -Width + 400, 140 + i * 40, Color.FromArgb(80,Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.DrawRect(-Width + 50, 100 + i * 40, -Width + 50 + 350f * score.Scoring.Judgements[i] / score.maxcombo, 140 + i * 40, Color.FromArgb(140, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.DrawText(Game.Options.Theme.Judges[i], 30, -Width+50, 100+i * 40, Color.White);
                SpriteBatch.DrawJustifiedText(score.Scoring.Judgements[i].ToString(), 30, -Width+400, 100+i * 40, Color.White);
            }
            SpriteBatch.DrawText(score.Scoring.BestCombo.ToString()+"x", 30, -Width + 50, 340, Color.White);
            SpriteBatch.DrawJustifiedText(score.Scoring.ComboBreaks.ToString()+ "cbs", 30, -Width + 400, 340, Color.White);
            DrawGraph();
        }

        private void DrawGraph()
        {
            SpriteBatch.DrawRect(-400, 200, 400, 400, Color.FromArgb(150, 0, 0, 0));
            float w = 800f / snapcount;
            float scale = 100f / score.Scoring.MissWindow;
            SpriteBatch.DrawRect(-400, 297, 400, 303, Color.Green);
            for (int i = 0; i < snapcount; i++)
            {
                for (int k = 0; k < score.hitdata[i].hit.Length; k++)
                {
                    if (score.hitdata[i].hit[k] == 1)
                    {
                        SpriteBatch.DrawRect(
                                -399 + i * w, 200, -401 + i * w, 400, Color.FromArgb(120, Game.Options.Theme.JudgeColors[5]));
                    }
                    else if (score.hitdata[i].hit[k] == 2)
                    {
                        float o = score.hitdata[i].delta[k];
                        SpriteBatch.DrawRect(
                                -398 + i * w, 298 - o * scale, -402 + i * w, 302 - o * scale, Game.Options.Theme.JudgeColors[score.Scoring.JudgeHit(Math.Abs(o))]);
                    }
                }
            }
        }
    }
}
