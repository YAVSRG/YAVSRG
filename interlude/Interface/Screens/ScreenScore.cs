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
        string[] mods;
        private ScoreTracker score;
        Sprite rank;
        int tier;
        int snapcount;
        ScoreSystem acc1, acc2;

        public ScreenScore(ScoreTracker data)
        {
            score = data;
            snapcount = score.c.Notes.Count;
            mods = Game.Gameplay.GetModifiers();
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

            ChartDifficulty c = new ChartDifficulty();
            c.PositionTopLeft(520, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(20, 0, AnchorType.MAX, AnchorType.MAX);
            AddChild(c);

            acc1 = ScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == ScoreType.Osu) ? ScoreType.Default : ScoreType.Osu);
            acc2 = ScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == ScoreType.Wife || Game.Options.Profile.ScoreSystem == ScoreType.DP) ? ScoreType.Default : ScoreType.Wife);
            acc1.ProcessScore(score.hitdata);
            acc2.ProcessScore(score.hitdata);

            Game.Options.Profile.Stats.SecondsPlayed += (int)(Game.CurrentChart.GetDuration() / 1000 / Game.Options.Profile.Rate);
            Game.Options.Profile.Stats.SRanks += (tier == 1 ? 1 : 0);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            //SpriteBatch.DrawCentredText(ranks[tier], 200f, 0, -320, rankColors[tier]);
            SpriteBatch.Draw(rank, -100, -280, 100, -80, Color.White);
            //you'll just have to change to the chart before showing the score screen
            //SpriteBatch.DrawCentredText(ChartLoader.SelectedChart.header.title + " [" + score.c.DifficultyName + "]", 30f, 0, -Height + 150, Color.White);
            //SpriteBatch.DrawCentredText(Utils.RoundNumber(Game.Options.Profile.Rate)+"x rate", 20f, 0, -Height + 200, Color.White);
            SpriteBatch.Font1.DrawCentredTextToFill(ChartLoader.SelectedChart.header.pack, left + 300, top, right - 300, top + 50, Game.Options.Theme.MenuFont);
            for (int i = 0; i < mods.Length; i++)
            {
                SpriteBatch.Font1.DrawText(mods[i], 30f, left, top + 10 + i * 40, Game.Options.Theme.MenuFont);
            }
            SpriteBatch.Font1.DrawCentredText(score.Scoring.FormatAcc(), 50, 0, -50, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredText(acc1.FormatAcc(), 30, -250, 50, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredText(acc2.FormatAcc(), 30, 250, 50, Game.Options.Theme.MenuFont);
            for (int i = 0; i < 6; i++)
            {
                SpriteBatch.DrawRect(left + 50, 100 + i * 40, left + 400, 140 + i * 40, Color.FromArgb(80, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.DrawRect(left + 50, 100 + i * 40, left + 50 + 350f * score.Scoring.Judgements[i] / score.maxcombo, 140 + i * 40, Color.FromArgb(140, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.Font2.DrawText(Game.Options.Theme.Judges[i], 30, left + 50, 100 + i * 40, Color.White);
                SpriteBatch.Font2.DrawJustifiedText(score.Scoring.Judgements[i].ToString(), 30, -ScreenWidth + 400, 100 + i * 40, Color.White);
            }
            SpriteBatch.Font1.DrawText(score.Scoring.BestCombo.ToString() + "x", 30, left + 50, 340, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(score.Scoring.ComboBreaks.ToString() + "cbs", 30, left + 400, 340, Game.Options.Theme.MenuFont);
            DrawGraph();
        }

        private void DrawGraph()
        {
            SpriteBatch.DrawRect(-400, 200, 400, 400, Color.FromArgb(150, 0, 0, 0));
            float w = 800f / snapcount;
            float scale = 100f / score.Scoring.MissWindow;
            SpriteBatch.DrawRect(-400, 297, 400, 303, Color.Green);
            int j;
            float o;
            for (int i = 0; i < snapcount; i++)
            {
                for (int k = 0; k < score.hitdata[i].hit.Length; k++)
                {
                    if (score.hitdata[i].hit[k] == 1)
                    {
                        SpriteBatch.DrawRect(
                                -399 + i * w, 200, -401 + i * w, 400, Color.FromArgb(80, Game.Options.Theme.JudgeColors[5]));
                    }
                    else if (score.hitdata[i].hit[k] == 2)
                    {
                        o = score.hitdata[i].delta[k];
                        j = score.Scoring.JudgeHit(o);
                        SpriteBatch.DrawRect(
                                -398 + i * w, 298 - o * scale, -402 + i * w, 302 - o * scale, Game.Options.Theme.JudgeColors[j]);
                    }
                }
            }
        }
    }
}
