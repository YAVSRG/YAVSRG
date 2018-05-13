using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap.DifficultyRating;
using System.Drawing;
using static YAVSRG.ChartLoader;

namespace YAVSRG.Interface.Widgets
{
    public class ChartInfoPanel : Widget //planned replacement for ChartDifficulty.
    {
        RatingReport diff;
        float physical;
        float technical;
        float rate;
        string time, bpm;
        Scoreboard sb;
        Animations.AnimationCounter anim;

        Sprite texture, frame;

        public ChartInfoPanel() : base()
        {
            ChangeChart();
            Animation.Add(anim = new Animations.AnimationCounter(400000, true));
            texture = Content.LoadTextureFromAssets("levelselectbase");
            frame = Content.LoadTextureFromAssets("frame");
        }

        public void ChangeChart()
        {
            diff = new RatingReport(Game.Gameplay.ModifiedChart, (float)Game.Options.Profile.Rate, 45f);
            rate = (float)Game.Options.Profile.Rate;
            physical = diff.breakdown[0];
            technical = diff.breakdown[1];
            time = Utils.FormatTime(Game.CurrentChart.GetDuration() / (float)Game.Options.Profile.Rate);
            bpm = ((int)(Game.CurrentChart.GetBPM() * Game.Options.Profile.Rate)).ToString() + "BPM";
            if (sb != null)
            {
                Widgets.Remove(sb);
            }
            sb = new Scoreboard();
            AddChild(sb.PositionTopLeft(50, 200, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(500, 150, AnchorType.MIN, AnchorType.MAX));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            ScreenUtils.DrawParallelogramWithBG(frame, left, top, right, top + 150, 0.5f);
            ScreenUtils.DrawParallelogramWithBG(frame, left, bottom - 100, right, bottom, -0.5f);
            SpriteBatch.Font1.DrawCentredTextToFill(SelectedChart.header.artist + " - " + SelectedChart.header.title, left, top, right, top + 100, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredTextToFill("Charted by " + SelectedChart.header.creator + "         From " + SelectedChart.header.pack, left + 50, top + 80, right - 50, top+150, Game.Options.Theme.MenuFont);

            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.DifficultyName, left + 550, top + 160, right - 50, top + 240, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawText("Physical", 20f, left + 550, top + 240, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawText(Utils.RoundNumber(physical) + "*", 40f, left + 550, top + 260, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawJustifiedText("Technical", 20f, right - 50, top + 240, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(Utils.RoundNumber(technical) + "*", 40f, right - 50, top + 260, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredTextToFill(Utils.RoundNumber(rate) + "x Audio", left + 650, top + 270, right - 150, top + 310, Game.Options.Theme.MenuFont);

            SpriteBatch.Font1.DrawText(time, 40f, left + 550, bottom - 220, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(bpm, 40f, right - 50, bottom - 220, Game.Options.Theme.MenuFont);

            //DrawGraph(left + 550, top + 350, right - 50, bottom - 250);

            DrawWidgets(left, top, right, bottom);
        }

        public void DrawGraph(float left, float top, float right, float bottom)
        {
            int c = diff.physical.Length;
            float x = (right - left) / c;
            float y = (bottom - top) / physical * 0.25f;
            for (int i = 0; i < c; i++)
            {
                SpriteBatch.DrawRect(left + x * i - 1, bottom - y * diff.physical[i] - 5, left + x * i + 1, bottom - y * diff.physical[i] + 5, Color.Aqua);
            }
            SpriteBatch.Font2.DrawCentredTextToFill("Replace with NPS graph?",left, top, right, bottom, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawFrame(frame, left, top, right, bottom, 30f, Color.White);
        }
    }
}
