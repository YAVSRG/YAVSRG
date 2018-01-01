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
        private PlayingChart score;
        static string[] ranks = new[] { "SS", "S", "A", "B", "C", "F" };
        static Color[] rankColors = new[] { Color.Gold, Color.Orange, Color.Green, Color.Blue, Color.Purple, Color.Gray };
        int tier;
        int cbs;
        int mapcombo;
        string osuacc;
        string dpacc;

        public ScreenScore(PlayingChart data)
        {
            score = data;
            mapcombo = score.c.States.Count;
            score.ComboBreak(); //it is important that this is here

            float acc = score.Accuracy();
            if (acc > 98) { tier = 0; } //code goes here for custom grade boundaries
            else if (acc > 95) { tier = 1; }
            else if (acc > 93) { tier = 2; }
            else if (acc > 91) { tier = 3; }
            else if (acc > 89) { tier = 4; }
            else { tier = 5; }

            ChartDifficulty c = new ChartDifficulty(Game.CurrentChart);
            c.PositionTopLeft(520, 605, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(20, 105, AnchorType.MAX, AnchorType.MAX);
            Widgets.Add(c);

            dpacc = Utils.RoundNumber(ScoreCalculator(new int[] { 2, 2, 1, -4, -8, -8 }, 2));
            osuacc = Utils.RoundNumber(ScoreCalculator(new int[] { 300, 300, 200, 100, 50, 0 }, 300));

            for (int i = 0; i < mapcombo; i++)
            {
                cbs += score.hitdata[i].angery;
            }
        }

        public float ScoreCalculator(int[] weights, int max)
        {
            int total = 0;
            int value = 0;
            PlayingChart.ChordCohesion c;
            for (int i = 0; i < mapcombo; i++)
            {
                foreach (int k in score.c.States.Points[i].Combine().GetColumns())
                {
                    c = score.hitdata[i];
                    if (c.hit[k])
                    {
                        value += weights[Game.Options.Profile.JudgeHit(Math.Abs(c.delta[k]))];
                    }
                    else
                    {
                        value += weights[5];
                    }
                    total += max;
                }
            }
            return 100f * value / total;
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Toolbar.hide = false;
        }

        public override void Draw()
        {
            base.Draw();
            SpriteBatch.DrawCentredText(ranks[tier], 200f, 0, -320, rankColors[tier]);
            //you'll just have to change to the chart before showing the score screen
            SpriteBatch.DrawCentredText(ChartLoader.SelectedChart.header.title + " [" + score.c.DifficultyName + "]", 30f, 0, -Height + 150, Color.White);
            SpriteBatch.DrawCentredText(Utils.RoundNumber(Game.Options.Profile.Rate)+"x rate", 20f, 0, -Height + 200, Color.White);
            SpriteBatch.DrawTextToFill(ChartLoader.SelectedPack.title, -Width+300, -Height + 80, Width-300, -Height + 130, Color.White);

            SpriteBatch.DrawCentredText(Utils.RoundNumber(score.Accuracy())+"%", 50, 0, -50, Color.White);
            SpriteBatch.DrawCentredText(osuacc + "% (Osu)", 30, -150, 50, Color.White);
            SpriteBatch.DrawCentredText(dpacc + "% (DP)", 30, 150, 50, Color.White);
            for (int i = 0; i < 6; i++)
            {
                SpriteBatch.DrawRect(-Width + 50, 100 + i * 40, -Width + 400, 140 + i * 40, Color.FromArgb(80,Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.DrawRect(-Width + 50, 100 + i * 40, -Width + 50 + 350f * score.judgement[i] / mapcombo, 140 + i * 40, Color.FromArgb(140, Game.Options.Theme.JudgeColors[i]));
                SpriteBatch.DrawText(Game.Options.Theme.Judges[i], 30, -Width+50, 100+i * 40, Color.White);
                SpriteBatch.DrawJustifiedText(score.judgement[i].ToString(), 30, -Width+400, 100+i * 40, Color.White);
            }
            SpriteBatch.DrawText(score.maxcombo.ToString()+"x", 30, -Width + 50, 340, Color.White);
            SpriteBatch.DrawJustifiedText(cbs.ToString()+ "cbs", 30, -Width + 400, 340, Color.White);
            DrawGraph();
        }

        private void DrawGraph()
        {
            SpriteBatch.DrawRect(-400, 200, 400, 400, Color.FromArgb(150, 0, 0, 0));
            float w = 800f / mapcombo;
            float scale = 30f / Game.Options.Profile.HitWindow;
            SpriteBatch.DrawRect(-400, 297, 400, 303, Color.Green);
            for (int i = 0; i < mapcombo; i++)
            {
                if (score.hitdata[i].angery > 0)
                {
                    SpriteBatch.DrawRect(
                            -399 + i * w, 200, -401 + i * w, 400, Color.FromArgb(120,Game.Options.Theme.JudgeColors[5]));
                }
                foreach (float o in score.hitdata[i].delta)
                {
                    if (o == 0) { continue; }
                    SpriteBatch.DrawRect(
                            -398 + i*w, 298 + o*scale, -402 + i*w, 302 + o*scale, Game.Options.Theme.JudgeColors[Game.Options.Profile.JudgeHit(Math.Abs(o))]);
                }
            }
        }
    }
}
