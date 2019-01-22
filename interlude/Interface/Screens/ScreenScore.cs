using System;
using System.Collections.Generic;
using System.Drawing;
using YAVSRG.IO;
using YAVSRG.Graphics;
using YAVSRG.Gameplay;
using YAVSRG.Gameplay.Watchers;
using YAVSRG.Gameplay.DifficultyRating;
using YAVSRG.Interface.Widgets;
using YAVSRG.Net.P2P.Protocol.Packets;
using static YAVSRG.Interface.ScreenUtils;

namespace YAVSRG.Interface.Screens
{
    class ScreenScore : Screen
    {
        static string[] ranks = new[] { "ss", "s", "a", "b", "c", "f" }; //todo: remove these in favour of more "modular" rank system
        string perf, badge;
        private ScoreTracker scoreData;
        int tier;
        int snapcount;
        IScoreSystem acc1, acc2;
        Scoreboard scoreboard;

        public ScreenScore(ScoreTracker data)
        {

            scoreData = data;
            snapcount = scoreData.Chart.Notes.Count;
            scoreData.Scoring.BestCombo = Math.Max(scoreData.Scoring.Combo, scoreData.Scoring.BestCombo); //if your biggest combo was until the end of the map, this catches it

            //awards the rank for your acc
            float acc = scoreData.Scoring.Accuracy();
            tier = 5;
            for (int i = 0; i < Game.Options.Profile.AccGradeThresholds.Length; i++) //custom grade boundaries
            {
                if (acc >= Game.Options.Profile.AccGradeThresholds[i])
                {
                    tier = i; break;
                }
            }

            //score saving logic including multiplayer handling
            Score score = new Score()
            {
                player = Game.Options.Profile.Name,
                time = DateTime.Now, hitdata = ScoreTracker.HitDataToString(scoreData.Hitdata),
                keycount = scoreData.Chart.Keys,
                mods = new Dictionary<string, string>(Game.Gameplay.SelectedMods),
                rate = (float)Game.Options.Profile.Rate,
                layout = Game.Options.Profile.KeymodeLayouts[scoreData.Chart.Keys]
            };
            if (ShouldSaveScore())
            {
                Game.Gameplay.ChartSaveData.Scores.Add(score);
                Game.Options.Profile.Stats.SetScore(score, Game.Gameplay.CurrentChart);
                Game.Gameplay.SaveScores();
            }
            if (Game.Multiplayer.SyncCharts)
            {
                Game.Multiplayer.SendPacket(new PacketScore() { score = score });
            }

            //update stats
            Game.Options.Profile.Stats.SecondsPlayed += (int)(Game.CurrentChart.GetDuration() / 1000 / Game.Options.Profile.Rate);
            Game.Options.Profile.Stats.SRanks += (tier == 1 ? 1 : 0);

            //alternative acc calculations
            acc1 = IScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == IScoreSystem.ScoreType.Osu) ? IScoreSystem.ScoreType.Default : IScoreSystem.ScoreType.Osu);
            acc2 = IScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == IScoreSystem.ScoreType.Wife || Game.Options.Profile.ScoreSystem == IScoreSystem.ScoreType.DP) ? IScoreSystem.ScoreType.Default : IScoreSystem.ScoreType.Wife);
            acc1.ProcessScore(scoreData.Hitdata);
            acc2.ProcessScore(scoreData.Hitdata);

            //more info pre calculated so it isn't calculated every frame
            perf = Utils.RoundNumber(PlayerRating.GetRating(Game.Gameplay.ChartDifficulty, scoreData.Hitdata));
            badge = Score.GetScoreBadge(scoreData.Scoring.Judgements);

            //build up UI
            scoreboard = new Scoreboard();
            scoreboard.UseScoreList(Game.Gameplay.ChartSaveData.Scores);
            AddChild(scoreboard.PositionTopLeft(50, 200, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(500, 50, AnchorType.MIN, AnchorType.MAX));
            AddChild(new ImageBox("rank-" + ranks[tier]).PositionTopLeft(-100, 150, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(100, 350, AnchorType.CENTER, AnchorType.MIN));
            AddChild(new ChartInfoPanel().PositionTopLeft(500, 350, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(50, 50, AnchorType.MAX, AnchorType.MAX));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Audio.OnPlaybackFinish = Game.Audio.Stop;
            PacketScoreboard.OnReceive += HandleMultiplayerScoreboard;
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            PacketScoreboard.OnReceive -= HandleMultiplayerScoreboard;
        }

        public bool ShouldSaveScore()
        {
            //other options i.e dont save if i get an F, dont save if i dont hp clear, dont save if i dont pb
            if (Game.Gameplay.GetModStatus(Game.Gameplay.SelectedMods) == 2) { return false; }
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
            SpriteBatch.Font1.DrawCentredTextToFill(scoreData.Scoring.FormatAcc(), new Rect(bounds.Left + 500, bounds.Top + 350, bounds.Right - 500, bounds.Top + 500), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font1.DrawCentredTextToFill(acc1.FormatAcc(), new Rect(bounds.Left + 550, bounds.Top + 500, bounds.CenterX - 50, bounds.Top + 600), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font1.DrawCentredTextToFill(acc2.FormatAcc(), new Rect(bounds.CenterX + 50, bounds.Top + 500, bounds.Right - 550, bounds.Top + 600), Game.Options.Theme.MenuFont, true);
            float r = 0;
            float h = 450/scoreData.Scoring.Judgements.Length;
            for (int i = 0; i < scoreData.Scoring.Judgements.Length; i++)
            {
                r = bounds.Right - 500 + i * h;
                SpriteBatch.DrawRect(new Rect(r, bounds.Top + 50, r + h, bounds.Top + 250), Color.FromArgb(80, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.DrawRect(new Rect(r, bounds.Top + 250 - 200f * scoreData.Scoring.Judgements[i] / scoreData.MaxCombo, r + h, bounds.Top + 250), Color.FromArgb(140, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.Font2.DrawCentredTextToFill(scoreData.Scoring.Judgements[i].ToString(), new Rect(r, bounds.Top + 50, r + h, bounds.Top + 150), Color.White, true);
                SpriteBatch.Font2.DrawCentredTextToFill(Utils.RoundNumber(scoreData.Scoring.Judgements[i] * 100f / scoreData.MaxCombo) + "%", new Rect(r, bounds.Top + 150, r + h, bounds.Top + 250), Color.White, true);
            }
            SpriteBatch.Font1.DrawTextToFill(scoreData.Scoring.BestCombo.ToString() + "x", new Rect(bounds.Right - 490, bounds.Top + 250, bounds.Right - 225, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font1.DrawCentredTextToFill(badge, new Rect(bounds.Right - 390, bounds.Top + 250, bounds.Right - 160, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            SpriteBatch.Font1.DrawJustifiedTextToFill(scoreData.Scoring.ComboBreaks.ToString() + "cbs", new Rect(bounds.Right - 225, bounds.Top + 250, bounds.Right - 60, bounds.Top + 300), Game.Options.Theme.MenuFont, true);
            DrawFrame(new Rect(bounds.Right - 500, bounds.Top + 50, bounds.Right - 50, bounds.Top + 300), 30f, Color.White);

            //graph
            DrawGraph(new Rect(bounds.Left + 550, bounds.Top + 600, bounds.Right - 550, bounds.Bottom - 50), scoreData.Scoring, scoreData.Hitdata);

            SpriteBatch.Font1.DrawTextToFill("Your performance:", new Rect(bounds.Left + 550, bounds.Top + 160, bounds.CenterX, bounds.Top + 180), Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawTextToFill(perf, new Rect(bounds.Left + 550, bounds.Top + 180, bounds.CenterX, bounds.Top + 250), Game.Options.Theme.MenuFont);
        }

        private void HandleMultiplayerScoreboard(PacketScoreboard packet, int id)
        {
            if (!Game.Multiplayer.SyncCharts) return;
            try
            {
                scoreboard.UseScoreList(packet.scores);
            }
            catch (Exception e)
            {
                Utilities.Logging.Log("Something went wrong displaying multiplayer scores", e.ToString(), Utilities.Logging.LogType.Error);
            }
        }
    }
}
