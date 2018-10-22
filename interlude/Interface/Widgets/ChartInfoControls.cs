using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.DifficultyRating;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    public class ChartInfoControls : Widget
    {
        Scoreboard sb;
        ChartInfoPanel ip;
        ScrollContainer scroll;
        object lastChart = null;

        public ChartInfoControls() : base()
        {
            sb = new Scoreboard();
            ip = new ChartInfoPanel();

            scroll = new ScrollContainer(10, 10, true, 1, false);

            AddChild(scroll.PositionTopLeft(50, 200, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(ScreenUtils.ScreenWidth * 2 - 750, 150, AnchorType.MIN, AnchorType.MAX));
            scroll.AddChild(sb.PositionBottomRight(-15, 20, AnchorType.CENTER, AnchorType.MAX));
            scroll.AddChild(ip.PositionBottomRight(-15, 20, AnchorType.CENTER, AnchorType.MAX));
            ChangeChart(true);
            
            ModMenu modMenu = new ModMenu();
            AddChild(modMenu.PositionTopLeft(0, 150, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(0, 100, AnchorType.MAX, AnchorType.MAX));

            AddChild(new FramedButton("buttonbase", "Play", () => { Game.Gameplay.PlaySelectedChart(); })
                .PositionTopLeft(0.55f, 75, AnchorType.LERP, AnchorType.MAX)
                .PositionBottomRight(0.9f, 25, AnchorType.LERP, AnchorType.MAX));
            AddChild(new FramedButton("buttonbase", "Mods", () => { modMenu.Toggle(); })
                .PositionTopLeft(0.1f, 75, AnchorType.LERP, AnchorType.MAX)
                .PositionBottomRight(0.45f, 25, AnchorType.LERP, AnchorType.MAX));
        }

        public override void OnResize()
        {
            base.OnResize();
            scroll.BottomRight.Reposition(ScreenUtils.ScreenWidth * 2 - 750, 150, AnchorType.MIN, AnchorType.MAX);
        }

        public void ChangeChart(bool force)
        {
            if (Game.Gameplay.CurrentChart != lastChart || force)
            {
                sb.UseScoreList(Game.Gameplay.ChartSaveData.Scores);
                lastChart = Game.Gameplay.CurrentChart;
            }
            scroll.BottomRight.Reposition(ScreenUtils.ScreenWidth * 2 - 750, 150, AnchorType.MIN, AnchorType.MAX); //just to make sure
            ip.ChangeChart(); //ip needs to change length/bpm. difficulty is already recalculated
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            //slice
            ScreenUtils.DrawParallelogramWithBG(new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + 150), 0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            ScreenUtils.DrawParallelogramWithBG(new Rect(bounds.Left, bounds.Bottom - 100, bounds.Right, bounds.Bottom), -0.5f, Game.Screens.DarkColor, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title, new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + 100), Game.Options.Theme.MenuFont, true, Color.Black);
            SpriteBatch.Font2.DrawCentredTextToFill("Charted by " + Game.CurrentChart.Data.Creator + "         From " + Game.CurrentChart.Data.SourcePack, new Rect(bounds.Left + 50, bounds.Top + 80, bounds.Right - 50, bounds.Top+150), Game.Options.Theme.MenuFont, true, Color.Black);
            
            //DrawGraph(bounds.Left + 550, bounds.Top + 350, bounds.Right - 50, bounds.Bottom - 250);

            DrawWidgets(bounds);
        }

        /*
        public void DrawGraph(float left, float top, float right, float bottom)
        {
            int c = diff.PhysicalData.Length;
            float x = (right - left) / c;
            float y = (bottom - top) / physical * 0.25f;
            for (int i = 0; i < c; i++)
            {
                //SpriteBatch.DrawRect(left + x * i - 1, bottom - y * diff.PhysicalData[i] - 5, left + x * i + 1, bottom - y * diff.PhysicalData[i] + 5, Color.Aqua);
            }
            SpriteBatch.Font2.DrawCentredTextToFill("Replace with NPS graph?",left, top, right, bottom, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawFrame(left, top, right, bottom, 30f, Color.White);
        }*/
    }
}
