using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using static YAVSRG.Interface.ScreenUtils;
using System.Drawing;
using YAVSRG.Interface.Widgets;
using YAVSRG.Net.P2P.Protocol.Packets;

namespace YAVSRG.Interface.Screens
{
    class ScreenScore : Screen
    {
        static string[] ranks = new[] { "ss", "s", "a", "b", "c", "f" };
        string mods, time, bpm, perf;
        private ScoreTracker scoreData;
        Sprite rank;
        int tier;
        int snapcount;
        ScoreSystem acc1, acc2;
        Scoreboard scoreboard;

        public ScreenScore(ScoreTracker data)
        {

            scoreData = data;
            snapcount = scoreData.c.Notes.Count;
            mods = Game.Gameplay.GetModString(Game.Gameplay.SelectedMods, (float)Game.Options.Profile.Rate, Game.Options.Profile.Playstyles[scoreData.c.Keys]);
            scoreData.Scoring.BestCombo = Math.Max(scoreData.Scoring.Combo, scoreData.Scoring.BestCombo); //if your biggest combo was until the end of the map, this catches it

            //awards the rank for your acc
            float acc = scoreData.Accuracy();
            tier = 5;
            for (int i = 0; i < Game.Options.Profile.AccGradeThresholds.Length; i++) //custom grade boundaries
            {
                if (acc >= Game.Options.Profile.AccGradeThresholds[i])
                {
                    tier = i; break;
                }
            }
            rank = Content.GetTexture("rank-" + ranks[tier]);

            //score saving logic including multiplayer
            Score score = new Score() { player = Game.Options.Profile.Name, time = DateTime.Now, hitdata = ScoreTracker.HitDataToString(scoreData.Hitdata), keycount = scoreData.c.Keys, mods = new Dictionary<string, string>(Game.Gameplay.SelectedMods), rate = (float)Game.Options.Profile.Rate, playstyle = Game.Options.Profile.Playstyles[scoreData.c.Keys] };

            if (ShouldSaveScore())
            {
                Game.Gameplay.ChartSaveData.Scores.Add(score);
                Game.Options.Profile.Stats.SetScore(score);
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
            acc1 = ScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == ScoreType.Osu) ? ScoreType.Default : ScoreType.Osu);
            acc2 = ScoreSystem.GetScoreSystem((Game.Options.Profile.ScoreSystem == ScoreType.Wife || Game.Options.Profile.ScoreSystem == ScoreType.DP) ? ScoreType.Default : ScoreType.Wife);
            acc1.ProcessScore(scoreData.Hitdata);
            acc2.ProcessScore(scoreData.Hitdata);

            //more info pre calculated so it isn't calculated every frame
            time = Utils.FormatTime(Game.CurrentChart.GetDuration() / (float)Game.Options.Profile.Rate);
            bpm = ((int)(Game.CurrentChart.GetBPM() * Game.Options.Profile.Rate)).ToString() + "BPM";
            perf = Utils.RoundNumber(Charts.DifficultyRating.PlayerRating.GetRating(Game.Gameplay.ChartDifficulty, scoreData.Hitdata));

            //build up UI
            scoreboard = new Scoreboard();
            scoreboard.UseScoreList(Game.Gameplay.ChartSaveData.Scores);
            AddChild(scoreboard.PositionTopLeft(50, 200, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(500, 50, AnchorType.MIN, AnchorType.MAX));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Audio.OnPlaybackFinish = () => { Game.Audio.Stop(); };
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
        
        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            if (Input.KeyTap(Game.Options.General.Binds.Screenshot))
            {
                Bitmap bm = Utils.CaptureWindow();
                System.Windows.Forms.Clipboard.SetImage(bm);
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);

            //top panel
            DrawParallelogramWithBG(left, top, right - 600, top + 150, 0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title, left, top, right - 600, top + 100, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredTextToFill("Charted by " + Game.CurrentChart.Data.Creator + "         From " + Game.CurrentChart.Data.SourcePack, left + 50, top + 80, right - 650, top + 150, Game.Options.Theme.MenuFont);

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
            SpriteBatch.DrawFrame(right - 500, top + 200, right - 50, r + h + h, 30f, Color.White);

            //middle stuff
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.DiffName, left + 550, top + 160, right - 550, top + 240, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredTextToFill(mods, left + 550, bottom - 450, right - 550, bottom - 350, Game.Options.Theme.MenuFont);
            SpriteBatch.Draw(rank, -100, -200, 100, 0, Color.White);
            SpriteBatch.Font1.DrawText(time, 40f, left + 550, bottom - 80, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(bpm, 40f, right - 550, bottom - 80, Game.Options.Theme.MenuFont);

            //graph
            DrawGraph(left + 550, bottom - 350, right - 550, bottom - 180, scoreData.Scoring, scoreData.Hitdata);

            SpriteBatch.Font1.DrawCentredText("Your performance", 30f, 0, bottom - 170, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredText(perf, 100f, 0, bottom - 145, Game.Options.Theme.MenuFont);
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
                Utilities.Logging.Log("Something went wrong displaying multiplayer scores: "+e.ToString(), Utilities.Logging.LogType.Error);
            }
        }
    }
}
