using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Interface.Widgets;
using System.Drawing;

namespace YAVSRG.Interface.Screens
{
    class ScreenMenu : Screen
    {
        private static readonly string[] splashes = new[] { "Yet Another Vertically Scrolling Rhythm Game", "Some funny flavourtext", "Based on the hit game osu!mania", "Pausers never win", "Winners never pause", "Timing is everything", "Where's the pause button?", "More arrows than a medieval army", "Skill not included", "Click play already", "JUST MASH", "A cool name for a rhythm game" };
        private string splash = splashes[new Random().Next(0, splashes.Length)];

        public ScreenMenu()
        {
            AddChild(
                new FramedButton("buttonbase", "Play", () => { Push(new ScreenLevelSelect()); })
                .PositionTopLeft(-100,-100,AnchorType.CENTER,AnchorType.CENTER)
                .PositionBottomRight(100,0,AnchorType.CENTER,AnchorType.CENTER)
                );
            AddChild(
                new FramedButton("buttonbase", "Options", () => { Push(new ScreenOptions()); })
                .PositionTopLeft(-100, 100, AnchorType.CENTER, AnchorType.CENTER)
                .PositionBottomRight(100, 200, AnchorType.CENTER, AnchorType.CENTER)
                );
            AddChild(
                new FramedButton("buttonbase", "Quit", () => { Pop(); })
                .PositionTopLeft(-100, 300, AnchorType.CENTER, AnchorType.CENTER)
                .PositionBottomRight(100, 400, AnchorType.CENTER, AnchorType.CENTER)
                );
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            splash = splashes[new Random().Next(0, splashes.Length)];
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            SpriteBatch.DrawCentredText("YAVSRG", 50f, 0, -300, Color.Aqua);
            SpriteBatch.DrawCentredText(splash, 20f, 0, -240, Color.Aqua);
        }
    }
}
