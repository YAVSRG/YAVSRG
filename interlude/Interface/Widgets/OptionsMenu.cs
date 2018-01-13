using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    class OptionsMenu : Widget
    {
        private List<Widget> Widgets;

        public OptionsMenu() : base()
        {
            Widgets = new List<Widget>();
            Widgets.Add(
                new Slider("Scroll speed", v => { Game.Options.Profile.ScrollSpeed = v; }, () => Game.Options.Profile.ScrollSpeed, 1, 4, 0.05f)
                .PositionTopLeft(100, 75, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(200, 125, AnchorType.MIN, AnchorType.MIN)
                );
            Widgets.Add(
                new Slider("Hit Position", v => { Game.Options.Theme.HitPosition = (int)v; }, () => Game.Options.Theme.HitPosition, -100, 400, 1)
                .PositionTopLeft(100, 175, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(200, 225, AnchorType.MIN, AnchorType.MIN)
                );
            Widgets.Add(
                new Slider("Column Width", v => { Game.Options.Theme.ColumnWidth = (int)v; }, () => Game.Options.Theme.ColumnWidth, 10, 500, 10)
                .PositionTopLeft(100, 275, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(200, 325, AnchorType.MIN, AnchorType.MIN)
                );

            Widgets.Add(
                new BoolPicker("Disable SV", Game.Options.Profile.FixedScroll, v => { Game.Options.Profile.FixedScroll = v; })
                .PositionTopLeft(350, 75, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(450, 125, AnchorType.MIN, AnchorType.MIN)
                );
            Widgets.Add(
                new BoolPicker("Arrows for 4k", Game.Options.Theme.UseArrowsFor4k, v => { Game.Options.Theme.UseArrowsFor4k = v; })
                .PositionTopLeft(350, 175, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(450, 225, AnchorType.MIN, AnchorType.MIN)
                );
            Widgets.Add(
                new BoolPicker("Enable note colors", Game.Options.Theme.UseColor, v => { Game.Options.Theme.UseColor = v; })
                .PositionTopLeft(350, 275, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(450, 325, AnchorType.MIN, AnchorType.MIN)
                );
            Widgets.Add(new KeyLayoutMenu().PositionTopLeft(0, 300, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(0, 100, AnchorType.MAX, AnchorType.MAX));
            /*
            Widgets.Add(
                new Slider("Columns in note texture", v => { Game.Options.Theme.UV_X = (int)v; }, () => Game.Options.Theme.UV_X, 1, 9, 1)
                .PositionTopLeft(600, 75, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(700, 125, AnchorType.MIN, AnchorType.MIN)
                );
            Widgets.Add(
                new Slider("Rows in note texture", v => { Game.Options.Theme.UV_Y = (int)v; }, () => Game.Options.Theme.UV_Y, 1, 9, 1)
                .PositionTopLeft(600, 175, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(700, 225, AnchorType.MIN, AnchorType.MIN)
                );
                */
            Widgets.Add(new Button("buttonbase", "Open Data Folder", () => {
                System.Diagnostics.Process.Start("file://"+Content.WorkingDirectory);
            }).PositionTopLeft(0, 100, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, right, bottom, Game.Options.Theme.Dark);
            foreach (Widget w in Widgets)
            {
                w.Draw(left, top, right, bottom);
            }
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            foreach (Widget w in Widgets)
            {
                w.Update(left, top, right, bottom);
            }
        }
    }
}
