using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap.DifficultyRating;
using System.Drawing;
using YAVSRG.Beatmap;

namespace YAVSRG.Interface.Widgets
{
    public class ChartDifficulty : Widget
    {
        RatingReport diff;
        float physical;
        float technical;
        float rate;
        string time, bpm;

        Sprite texture, frame;

        public ChartDifficulty() : base()
        {
            ChangeChart();
            texture = Content.LoadTextureFromAssets("infocard");
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
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            Game.Screens.DrawStaticChartBackground(left, top, right, bottom, Color.Gray);
            //SpriteBatch.DrawTilingTexture(texture, left, top, right, bottom, 400f, 0, 0, Game.Screens.BaseColor);
            SpriteBatch.DrawFrame(frame, left, top, right, bottom, 30f, Color.White);
            SpriteBatch.DrawCentredTextToFill(ChartLoader.SelectedChart.header.title, left, top, right, top + 100, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawCentredTextToFill(Game.CurrentChart.DifficultyName, left, top + 110, right, top + 150, Game.Options.Theme.MenuFont);

            SpriteBatch.DrawText("Physical", 20f, left + 20, top + 160, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText("Technical", 20f, right - 20, top + 160, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawText(Utils.RoundNumber(physical)+"*", 40f, left + 20, top + 190, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText(Utils.RoundNumber(technical)+"*", 40f, right - 20, top + 190, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawCentredTextToFill(Utils.RoundNumber(rate) + "x Audio", left, top + 250, right, top + 300, Game.Options.Theme.MenuFont);

            SpriteBatch.DrawText(time, 40f, left + 20, bottom - 70, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawJustifiedText(bpm, 40f, right - 20, bottom - 70, Game.Options.Theme.MenuFont);

            string[] text = new[] { "MORE RELEVANT INFORMATION", ChartLoader.SelectedChart.header.pack};
            for (int i = 0; i < 2; i++)
            {
                SpriteBatch.DrawCentredText(text[i], 15f, (left + right) / 2, top + 340 + i * 60, Game.Options.Theme.MenuFont);
            }
        }
    }
}
