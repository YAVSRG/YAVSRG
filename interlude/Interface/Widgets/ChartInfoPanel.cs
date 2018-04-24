using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap.DifficultyRating;
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
            AddChild(sb.PositionTopLeft(25, 310, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(25, 100, AnchorType.MAX, AnchorType.MAX));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawTilingTexture(texture, left, top, right, bottom, 400f, 0, anim.value/1000f, Game.Screens.BaseColor);
            Game.Screens.DrawStaticChartBackground(left, top, right, bottom, Color.FromArgb(127,255,255,255));
            SpriteBatch.DrawFrame(frame, left, top, right, bottom, 30f, Color.White);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.DifficultyName, left, top, right, top + 100, Game.Options.Theme.MenuFont);

            SpriteBatch.Font2.DrawText("Physical", 20f, left + 20, top + 160, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawJustifiedText("Technical", 20f, right - 20, top + 160, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawText(Utils.RoundNumber(physical) + "*", 40f, left + 20, top + 190, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(Utils.RoundNumber(technical) + "*", 40f, right - 20, top + 190, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredTextToFill(Utils.RoundNumber(rate) + "x Audio", left, top + 250, right, top + 300, Game.Options.Theme.MenuFont);

            SpriteBatch.Font1.DrawText(time, 40f, left + 20, bottom - 70, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedText(bpm, 40f, right - 20, bottom - 70, Game.Options.Theme.MenuFont);

            DrawWidgets(left, top, right, bottom);
        }
    }
}
