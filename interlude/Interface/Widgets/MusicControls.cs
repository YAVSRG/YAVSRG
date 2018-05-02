using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    class MusicControls : Widget
    {
        Sprite frame;

        public MusicControls()
        {
            frame = Content.LoadTextureFromAssets("frame");
            AddChild(
                new Button("buttonplay", "", () => { Game.Audio.Play(); })
                .PositionTopLeft(250, 10, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(170, 90, AnchorType.MAX, AnchorType.MIN)
                );
            AddChild(
                new Button("buttonpause", "", () => { Game.Audio.Pause(); })
                .PositionTopLeft(170, 10, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(90, 90, AnchorType.MAX, AnchorType.MIN)
                );
            AddChild(
                new Button("buttonstop", "", () => { Game.Audio.Stop(); })
                .PositionTopLeft(90, 10, AnchorType.MAX, AnchorType.MIN)
                .PositionBottomRight(10, 90, AnchorType.MAX, AnchorType.MIN)
                );
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            Game.Screens.DrawChartBackground(left, top, right, bottom, Game.Screens.DarkColor, 0.25f);
            SpriteBatch.DrawFrame(frame, left, top, right, bottom, 30f, Game.Screens.HighlightColor);
            SpriteBatch.DrawRect(left + 40, bottom-30, left + 40 + (right - 300 - left) * Game.Audio.NowPercentage(), bottom - 20, Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredTextToFill(ChartLoader.SelectedChart.header.artist + " - " + ChartLoader.SelectedChart.header.title, left + 10, top, right - 260, top + 70, Game.Options.Theme.MenuFont);

            DrawWidgets(left, top, right, bottom);
        }
    }
}
