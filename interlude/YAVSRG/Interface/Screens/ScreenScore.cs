using System;
using System.Collections.Generic;
using System.Drawing;
using Prelude.Utilities;
using Prelude.Gameplay.DifficultyRating;
using Prelude.Gameplay;
using Prelude.Net.Protocol.Packets;
using Interlude.IO;
using Interlude.Graphics;
using Interlude.Gameplay;
using Interlude.Interface.Widgets;
using static Interlude.Interface.ScreenUtils;

namespace Interlude.Interface.Screens
{
    class ScreenScore : Screen
    {
        string badge; //todo: put this inside ScoreInfoProvider
        private ScoreInfoProvider scoreData;
        int noteCount;
        int rankachieved;
        Scoreboard scoreboard;
        ScoreGraph graph;

        public ScreenScore(ScoreTracker data)
        {
            noteCount = data.MaxPossibleCombo;

            //score saving logic including multiplayer handling
            Score score = new Score()
            {
                player = Game.Options.Profile.Name,
                time = DateTime.Now,
                hitdata = ScoreTracker.HitDataToString(data.Hitdata),
                keycount = data.Chart.Keys,
                selectedMods = new Dictionary<string, DataGroup>(Game.Gameplay.SelectedMods),
                rate = (float)Game.Options.Profile.Rate,
                layout = Game.Options.Profile.Playstyles[data.Chart.Keys - 3]
            };

            scoreData = new ScoreInfoProvider(score, Game.CurrentChart);
            scoreData.SetData(Game.Gameplay.ChartDifficulty, Game.Gameplay.GetModString(), data.Scoring, data.HP);

            if (ShouldSaveScore())
            {
                Game.Gameplay.ChartSaveData.Scores.Add(score);
                Game.Options.Profile.Stats.SetScore(score, Game.Gameplay.CurrentChart);
                Game.Gameplay.SaveScores();
                if (Game.Online.Connected)
                {
                    Game.Online.SendPacket(new PacketScore() { score = score, chartHash = Game.Gameplay.CurrentChart.GetHash() });
                }
            }

            //update stats
            Game.Options.Profile.Stats.SecondsPlayed += (int)(Game.CurrentChart.GetDuration() / 1000 / Game.Options.Profile.Rate);
            //Game.Options.Profile.Stats.SRanks += (rankachieved == 1 ? 1 : 0);
            badge = Score.GetScoreBadge(data.Scoring.Judgements);

            //build up UI
            scoreboard = new Scoreboard();
            scoreboard.UseScoreList(Game.Gameplay.ChartSaveData.Scores);
            AddChild(scoreboard.Reposition(50, 0, 200, 0, 500, 0, -50, 1));
            AddChild(new ChartInfoPanel().Reposition(-500, 1, 350, 0, -50, 1, -50, 1));
            AddChild((graph = new ScoreGraph(scoreData)).Reposition(550, 0, -350, 1, -550, 1, -50, 1));

            OnScoreSystemChanged();
        }

        void OnScoreSystemChanged()
        {
            badge = Score.GetScoreBadge(scoreData.ScoreSystem.Judgements);

            //awards the rank for your acc
            float acc = scoreData.ScoreSystem.Accuracy();
            rankachieved = Game.Options.Profile.GradeThresholds.Length;
            for (int i = 0; i < Game.Options.Profile.GradeThresholds.Length; i++) //custom grade boundaries
            {
                if (acc >= Game.Options.Profile.GradeThresholds[i])
                {
                    rankachieved = i; break;
                }
            }

            graph.RequestRedraw();
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Audio.OnPlaybackFinish = Game.Audio.Stop;
            Game.Screens.Toolbar.Icons.Filter(0b00000001);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            graph.RequestRedraw(); //releases FBO
            graph.SetState(WidgetState.DISABLED); //so it doesnt draw while the screen fades out and reuse the FBO
        }

        public bool ShouldSaveScore()
        {
            //other options i.e dont save if i get an F, dont save if i dont hp clear, dont save if i dont pb
            if (Game.Gameplay.ModifiedChart.ModStatus == 2) { return false; }
            else if (Game.Options.Profile.ScoreSavingPreference == ScoreSavingPreference.PASS) { return !scoreData.HP.HasFailed(); }
            else if (Game.Options.Profile.ScoreSavingPreference == ScoreSavingPreference.PACEMAKER) { return scoreData.ScoreSystem.Accuracy() >= Game.Options.Profile.Pacemaker; }
            return true;
        }
        
        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Game.Options.General.Hotkeys.Screenshot.Tapped())
            {
                Bitmap bm = Utils.CaptureWindow();
                System.Windows.Forms.Clipboard.SetImage(bm);
            }
            else if (Game.Options.General.Hotkeys.Previous.Tapped())
            {
                Game.Options.Profile.SelectedScoreSystem = Utils.Modulus(Game.Options.Profile.SelectedScoreSystem - 1, Game.Options.Profile.ScoreSystems.Count);
                scoreData.OnChangeScoreSystem();
                OnScoreSystemChanged();
            }
            else if (Game.Options.General.Hotkeys.Next.Tapped())
            {
                Game.Options.Profile.SelectedScoreSystem = Utils.Modulus(Game.Options.Profile.SelectedScoreSystem + 1, Game.Options.Profile.ScoreSystems.Count);
                scoreData.OnChangeScoreSystem();
                OnScoreSystemChanged();
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
            SpriteBatch.Font1.DrawCentredTextToFill(scoreData.ScoreSystem.FormatAcc(), new Rect(bounds.Left + 500, bounds.Top + 370, bounds.Right - 500, bounds.Top + 500), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture("ranks"), new Rect(-100, bounds.Top + 170, 100, bounds.Top + 370), scoreData.HP.HasFailed() ? Color.Gray : Color.White, rankachieved, 0));
            if (scoreData.HP.HasFailed()) SpriteBatch.Font1.DrawCentredTextToFill("Failed", new Rect(-100, bounds.Top + 170, 100, bounds.Top + 370), Color.Red, true, Color.Yellow);
            float h = 450/scoreData.ScoreSystem.HitTypes.Length;
            for (int j = 0; j < scoreData.ScoreSystem.HitTypes.Length; j++)
            {
                int i = (int)scoreData.ScoreSystem.HitTypes[j];
                float r = bounds.Right - 500 + j * h;
                SpriteBatch.DrawRect(new Rect(r, bounds.Top + 50, r + h, bounds.Top + 250), Color.FromArgb(80, Game.Options.Theme.JudgementColors[i]));
                SpriteBatch.DrawRect(new Rect(r, bounds.Top + 250 - 200f * scoreData.ScoreSystem.Judgements[i] / noteCount, r + h, bounds.Top + 250), Color.FromArgb(140, Game.Options.Theme.JudgementColors[i]));
                SpriteBatch.Font2.DrawCentredTextToFill(scoreData.ScoreSystem.Judgements[i].ToString(), new Rect(r, bounds.Top + 50, r + h, bounds.Top + 150), Color.White, true);
                SpriteBatch.Font2.DrawCentredTextToFill(Utils.RoundNumber(scoreData.ScoreSystem.Judgements[i] * 100f / noteCount) + "%", new Rect(r, bounds.Top + 150, r + h, bounds.Top + 250), Color.White, true);
            }
            SpriteBatch.Font1.DrawTextToFill(scoreData.ScoreSystem.BestCombo.ToString() + "x", new Rect(bounds.Right - 490, bounds.Top + 250, bounds.Right - 225, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font1.DrawCentredTextToFill(badge, new Rect(bounds.Right - 390, bounds.Top + 250, bounds.Right - 160, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font1.DrawJustifiedTextToFill(scoreData.ScoreSystem.ComboBreaks.ToString() + "cbs", new Rect(bounds.Right - 225, bounds.Top + 250, bounds.Right - 60, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            DrawFrame(new Rect(bounds.Right - 500, bounds.Top + 50, bounds.Right - 50, bounds.Top + 300), Color.White);

            SpriteBatch.Font1.DrawTextToFill("Your performance:", new Rect(bounds.Left + 550, bounds.Top + 160, bounds.CenterX, bounds.Top + 180), Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawTextToFill(Utils.RoundNumber(scoreData.PhysicalPerformance), new Rect(bounds.Left + 550, bounds.Top + 180, bounds.CenterX, bounds.Top + 250), Game.Options.Theme.MenuFont);
        }
    }
}
