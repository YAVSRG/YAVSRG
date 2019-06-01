using System;
using System.Collections.Generic;
using System.Drawing;
using Prelude.Utilities;
using Prelude.Gameplay.DifficultyRating;
using Prelude.Gameplay;
using Interlude.IO;
using Interlude.Graphics;
using Interlude.Gameplay;
using Interlude.Interface.Widgets;
using static Interlude.Interface.ScreenUtils;

namespace Interlude.Interface.Screens
{
    class ScreenScore : Screen
    {
        string perf, badge;
        private ScoreTracker scoreData;
        int rankachieved;
        Scoreboard scoreboard;

        public ScreenScore(ScoreTracker data)
        {

            scoreData = data;
            //awards the rank for your acc
            float acc = scoreData.Scoring.Accuracy();
            rankachieved = Game.Options.Profile.GradeThresholds.Length;
            for (int i = 0; i < Game.Options.Profile.GradeThresholds.Length; i++) //custom grade boundaries
            {
                if (acc >= Game.Options.Profile.GradeThresholds[i])
                {
                    rankachieved = i; break;
                }
            }

            //score saving logic including multiplayer handling
            Score score = new Score()
            {
                player = Game.Options.Profile.Name,
                time = DateTime.Now, hitdata = ScoreTracker.HitDataToString(scoreData.Hitdata),
                keycount = scoreData.Chart.Keys,
                selectedMods = new Dictionary<string, DataGroup>(Game.Gameplay.SelectedMods),
                rate = (float)Game.Options.Profile.Rate,
                layout = Game.Options.Profile.KeymodeLayouts[scoreData.Chart.Keys]
            };
            if (ShouldSaveScore())
            {
                Game.Gameplay.ChartSaveData.Scores.Add(score);
                Game.Options.Profile.Stats.SetScore(score, Game.Gameplay.CurrentChart);
                Game.Gameplay.SaveScores();
            }

            //update stats
            Game.Options.Profile.Stats.SecondsPlayed += (int)(Game.CurrentChart.GetDuration() / 1000 / Game.Options.Profile.Rate);
            Game.Options.Profile.Stats.SRanks += (rankachieved == 1 ? 1 : 0);

            //more info pre calculated so it isn't calculated every frame
            perf = Utils.RoundNumber(PlayerRating.GetRating(Game.Gameplay.ChartDifficulty, scoreData.Hitdata));
            badge = Score.GetScoreBadge(scoreData.Scoring.Judgements);

            //build up UI
            scoreboard = new Scoreboard();
            scoreboard.UseScoreList(Game.Gameplay.ChartSaveData.Scores);
            AddChild(scoreboard.TL_DeprecateMe(50, 200, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(500, 50, AnchorType.MIN, AnchorType.MAX));
            AddChild(new ChartInfoPanel().TL_DeprecateMe(500, 350, AnchorType.MAX, AnchorType.MIN).BR_DeprecateMe(50, 50, AnchorType.MAX, AnchorType.MAX));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Audio.OnPlaybackFinish = Game.Audio.Stop;
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
        }

        public bool ShouldSaveScore()
        {
            //other options i.e dont save if i get an F, dont save if i dont hp clear, dont save if i dont pb
            if (scoreData.Chart.ModStatus == 2) { return false; }
            return true;
        }
        
        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Input.KeyTap(Game.Options.General.Binds.Screenshot))
            {
                Bitmap bm = Utils.CaptureWindow();
                System.Windows.Forms.Clipboard.SetImage(bm);
            }
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            //todo: rewrite with text boxes

            //top panel
            DrawParallelogramWithBG(new Rect(bounds.Left, bounds.Top, bounds.Right - 600, bounds.Top + 150), 0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title, new Rect(bounds.Left, bounds.Top, bounds.Right - 600, bounds.Top + 100), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font2.DrawCentredTextToFill("Charted by " + Game.CurrentChart.Data.Creator + "         From " + Game.CurrentChart.Data.SourcePack, new Rect(bounds.Left + 50, bounds.Top + 80, bounds.Right - 650, bounds.Top + 150), Game.Options.Theme.MenuFont, true);

            //judgements display
            SpriteBatch.Font1.DrawCentredTextToFill(scoreData.Scoring.FormatAcc(), new Rect(bounds.Left + 500, bounds.Top + 370, bounds.Right - 500, bounds.Top + 500), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Draw("ranks", new Rect(-100, bounds.Top + 170, 100, bounds.Top + 370), Color.White, rankachieved, 0);
            float h = 450/scoreData.Scoring.Judgements.Length;
            for (int i = 0; i < scoreData.Scoring.Judgements.Length; i++)
            {
                float r = bounds.Right - 500 + i * h;
                SpriteBatch.DrawRect(new Rect(r, bounds.Top + 50, r + h, bounds.Top + 250), Color.FromArgb(80, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.DrawRect(new Rect(r, bounds.Top + 250 - 200f * scoreData.Scoring.Judgements[i] / scoreData.MaxCombo, r + h, bounds.Top + 250), Color.FromArgb(140, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.Font2.DrawCentredTextToFill(scoreData.Scoring.Judgements[i].ToString(), new Rect(r, bounds.Top + 50, r + h, bounds.Top + 150), Color.White, true);
                SpriteBatch.Font2.DrawCentredTextToFill(Utils.RoundNumber(scoreData.Scoring.Judgements[i] * 100f / scoreData.MaxCombo) + "%", new Rect(r, bounds.Top + 150, r + h, bounds.Top + 250), Color.White, true);
            }
            SpriteBatch.Font1.DrawTextToFill(scoreData.Scoring.BestCombo.ToString() + "x", new Rect(bounds.Right - 490, bounds.Top + 250, bounds.Right - 225, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font1.DrawCentredTextToFill(badge, new Rect(bounds.Right - 390, bounds.Top + 250, bounds.Right - 160, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font1.DrawJustifiedTextToFill(scoreData.Scoring.ComboBreaks.ToString() + "cbs", new Rect(bounds.Right - 225, bounds.Top + 250, bounds.Right - 60, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            DrawFrame(new Rect(bounds.Right - 500, bounds.Top + 50, bounds.Right - 50, bounds.Top + 300), Color.White);

            //graph
            DrawGraph(new Rect(bounds.Left + 550, bounds.Top + 600, bounds.Right - 550, bounds.Bottom - 50), scoreData.Scoring, scoreData.Hitdata);

            SpriteBatch.Font1.DrawTextToFill("Your performance:", new Rect(bounds.Left + 550, bounds.Top + 160, bounds.CenterX, bounds.Top + 180), Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawTextToFill(perf, new Rect(bounds.Left + 550, bounds.Top + 180, bounds.CenterX, bounds.Top + 250), Game.Options.Theme.MenuFont);
        }
    }
}
