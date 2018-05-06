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
        string mods, time, bpm;
        private ScoreTracker scoreData;
        Sprite rank, frame;
        int tier;
        int snapcount;
        ScoreSystem acc1, acc2;

        public ScreenScore(ScoreTracker data)
        {
            scoreData = data;
            snapcount = scoreData.c.Notes.Count;
            mods = string.Join(", ",Game.Gameplay.GetModifiers());
            scoreData.Scoring.BestCombo = Math.Max(scoreData.Scoring.Combo, scoreData.Scoring.BestCombo); //if your biggest combo was until the end of the map, this catches it

            float acc = scoreData.Accuracy();
            tier = 5;
            for (int i = 0; i < Game.Options.Profile.AccGradeThresholds.Length; i++) //custom grade boundaries
            {
                if (acc >= Game.Options.Profile.AccGradeThresholds[i])
                {
                    tier = i; break;
                }
            }
            rank = Content.LoadTextureFromAssets("rank-" + ranks[tier]);
            frame = Content.LoadTextureFromAssets("frame");
            Game.Gameplay.ChartSaveData.TEMP_SCORES2.Add(new Score() { player = Game.Options.Profile.Name, date = DateTime.Now.ToShortDateString(), hitdata = ScoreTracker.HitDataToString(scoreData.hitdata), keycount = scoreData.c.Keys, mods = Game.Gameplay.GetModifiers().ToList(), time = DateTime.Now.ToShortTimeString() });

            acc1 = ScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == ScoreType.Osu) ? ScoreType.Default : ScoreType.Osu);
            acc2 = ScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == ScoreType.Wife || Game.Options.Profile.ScoreSystem == ScoreType.DP) ? ScoreType.Default : ScoreType.Wife);
            acc1.ProcessScore(scoreData.hitdata);
            acc2.ProcessScore(scoreData.hitdata);

            time = Utils.FormatTime(Game.CurrentChart.GetDuration() / (float)Game.Options.Profile.Rate);
            bpm = ((int)(Game.CurrentChart.GetBPM() * Game.Options.Profile.Rate)).ToString() + "BPM";

            AddChild(new Scoreboard().PositionTopLeft(50, 200, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(500, 50, AnchorType.MIN, AnchorType.MAX));

            Game.Options.Profile.Stats.SecondsPlayed += (int)(Game.CurrentChart.GetDuration() / 1000 / Game.Options.Profile.Rate);
            Game.Options.Profile.Stats.SRanks += (tier == 1 ? 1 : 0);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            //you'll just have to change to the chart before showing the score screen <- dont worry old me, this always happens :)

            //top panel
            DrawParallelogramWithBG(frame, left, top, right - 600, top + 150, 0.5f);
            SpriteBatch.Font1.DrawCentredTextToFill(ChartLoader.SelectedChart.header.artist + " - " + ChartLoader.SelectedChart.header.title, left, top, right - 600, top + 100, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredTextToFill("Charted by " + ChartLoader.SelectedChart.header.creator + "         From " + ChartLoader.SelectedChart.header.pack, left + 50, top + 80, right - 650, top + 150, Game.Options.Theme.MenuFont);

            //judgements display
            SpriteBatch.Font1.DrawCentredTextToFill(scoreData.Scoring.FormatAcc(), right - 500, top + 20, right - 50, top + 150, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredTextToFill(acc1.FormatAcc(), right - 500, top + 140, right - 275, top + 200, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredTextToFill(acc2.FormatAcc(), right - 275, top + 140, right - 50, top + 200, Game.Options.Theme.MenuFont);
            float r = 0;
            float h = (bottom - 250 - top) / 7;
            for (int i = 0; i < 6; i++)
            {
                r = top + 200 + i * h;
                SpriteBatch.DrawRect(right - 500, r, right - 50, r + h, Color.FromArgb(80, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.DrawRect(right - 500, r, right - 500 + 450f * scoreData.Scoring.Judgements[i] / scoreData.maxcombo, r + h, Color.FromArgb(140, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.Font2.DrawTextToFill(Game.Options.Theme.Judges[i], right - 500, r, right - 250, r + h, Color.White);
                SpriteBatch.Font2.DrawJustifiedTextToFill(
                    "(" + Utils.RoundNumber(scoreData.Scoring.Judgements[i] * 100f / scoreData.maxcombo) + "%) " + scoreData.Scoring.Judgements[i].ToString(),
                    right - 250, r, right - 50, r + h, Color.White);
            }
            SpriteBatch.Font1.DrawTextToFill(scoreData.Scoring.BestCombo.ToString() + "x", right - 500, r + h, right - 225, r + h + h, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedTextToFill(scoreData.Scoring.ComboBreaks.ToString() + "cbs", right - 225, r + h, right - 50, r + h + h, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawFrame(frame, right - 500, top + 200, right - 50, r + h + h, 30f, Color.White);

            //middle stuff
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.DifficultyName, left + 550, top + 160, right - 550, top + 240, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredTextToFill(mods, left + 550, bottom - 450, right - 550, bottom - 350, Game.Options.Theme.MenuFont);
            SpriteBatch.Draw(rank, -100, -200, 100, 0, Color.White);
            SpriteBatch.Font1.DrawText(time, 40f, left + 550, bottom - 80, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(bpm, 40f, right - 550, bottom - 80, Game.Options.Theme.MenuFont);

            //graph
            DrawGraph(left + 550, bottom - 350, right - 550, bottom - 150);
        }

        private void DrawGraph(float left, float top, float right, float bottom)
        {
            SpriteBatch.DrawRect(left, top, right, bottom, Color.FromArgb(150, 0, 0, 0));
            float w = (right - left) / snapcount;
            float middle = (top + bottom) * 0.5f;
            float scale = (bottom - top) * 0.5f / scoreData.Scoring.MissWindow;
            SpriteBatch.DrawRect(left, middle - 3, right, middle + 3, Color.Green);
            int j;
            float o;
            for (int i = 0; i < snapcount; i++)
            {
                for (int k = 0; k < scoreData.hitdata[i].hit.Length; k++)
                {
                    if (scoreData.hitdata[i].hit[k] > 0)
                    {
                        o = scoreData.hitdata[i].delta[k];
                        j = scoreData.Scoring.JudgeHit(o);
                        if (j > 2)
                        {
                            SpriteBatch.DrawRect(left + i * w - 1, top, left + i * w + 1, bottom, Color.FromArgb(80, Game.Options.Theme.JudgeColors[5]));
                        }
                        SpriteBatch.DrawRect(left + i * w - 2, middle - o * scale - 2, left + i * w + 2, middle - o * scale + 2, Game.Options.Theme.JudgeColors[j]);
                    }
                }
            }
            SpriteBatch.DrawFrame(frame, left, top, right, bottom, 30f, Color.White);
        }
    }
}
