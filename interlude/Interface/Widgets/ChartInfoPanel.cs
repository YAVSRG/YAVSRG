using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.DifficultyRating;
using System.Drawing;

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
            texture = Content.GetTexture("levelselectbase");
            frame = Content.GetTexture("frame");
        }

        public void ChangeChart()
        {
            diff = Game.Gameplay.ChartDifficulty;
            rate = (float)Game.Options.Profile.Rate;
            physical = diff.Physical;
            technical = diff.Technical;
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
            ScreenUtils.DrawParallelogramWithBG(left, top, right, top + 150, 0.5f);
            ScreenUtils.DrawParallelogramWithBG(left, bottom - 100, right, bottom, -0.5f);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title, left, top, right, top + 100, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredTextToFill("Charted by " + Game.CurrentChart.Data.Creator + "         From " + Game.CurrentChart.Data.SourcePack, left + 50, top + 80, right - 50, top+150, Game.Options.Theme.MenuFont);

            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.DiffName, left + 550, top + 160, right - 50, top + 240, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawText("Physical", 20f, left + 550, top + 240, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawText(Utils.RoundNumber(physical) + "⋆", 40f, left + 550, top + 260, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawJustifiedText("Technical", 20f, right - 50, top + 240, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(Utils.RoundNumber(technical) + "⋆", 40f, right - 50, top + 260, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredTextToFill(Utils.RoundNumber(rate) + "x Audio", left + 650, top + 270, right - 150, top + 310, Game.Options.Theme.MenuFont);

            SpriteBatch.Font1.DrawText(time, 40f, left + 550, bottom - 220, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(bpm, 40f, right - 50, bottom - 220, Game.Options.Theme.MenuFont);

            //DrawGraph(left + 550, top + 350, right - 50, bottom - 250);

            DrawWidgets(left, top, right, bottom);
        }

        public void DrawGraph(float left, float top, float right, float bottom)
        {
            int c = diff.PhysicalData.Length;
            float x = (right - left) / c;
            float y = (bottom - top) / physical * 0.25f;
            for (int i = 0; i < c; i++)
            {
                SpriteBatch.DrawRect(left + x * i - 1, bottom - y * diff.PhysicalData[i] - 5, left + x * i + 1, bottom - y * diff.PhysicalData[i] + 5, Color.Aqua);
            }
            SpriteBatch.Font2.DrawCentredTextToFill("Replace with NPS graph?",left, top, right, bottom, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawFrame(left, top, right, bottom, 30f, Color.White);
        }
    }
}
