using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;
using YAVSRG.Interface;
using YAVSRG.Gameplay;

namespace YAVSRG.Options.Tabs
{
    class GameplayTab : WidgetContainer
    {
        public GameplayTab()
        {
            Widgets.Add(
                new Slider("Scroll speed", v => { Game.Options.Profile.ScrollSpeed = v; }, () => Game.Options.Profile.ScrollSpeed, 1, 4, 0.01f)
                .PositionTopLeft(-100, 75, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(100, 125, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new Slider("Hit Position", v => { Game.Options.Profile.HitPosition = (int)v; }, () => Game.Options.Profile.HitPosition, -100, 400, 1)
                .PositionTopLeft(-100, 175, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(100, 225, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new Slider("Upwards Screencover", v => { Game.Options.Profile.ScreenCoverUp = v; }, () => Game.Options.Profile.ScreenCoverUp, 0, 1, 0.05f)
                .PositionTopLeft(-300, 275, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 325, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new Slider("Downwards Screencover", v => { Game.Options.Profile.ScreenCoverDown = v; }, () => Game.Options.Profile.ScreenCoverDown, 0, 1, 0.05f)
                .PositionTopLeft(50, 275, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(300, 325, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new BoolPicker("Disable SV", Game.Options.Profile.FixedScroll, v => { Game.Options.Profile.FixedScroll = v; })
                .PositionTopLeft(-200, 375, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 425, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new BoolPicker("Arrows for 4k", Game.Options.Profile.UseArrowsFor4k, v => { Game.Options.Profile.UseArrowsFor4k = v; })
                .PositionTopLeft(50, 375, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(200, 425, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new TextPicker("Note Color Style", new string[] { "DDR", "Column", "Chord" }, (int)Game.Options.Profile.ColorStyle.Style, v => { Game.Options.Profile.ColorStyle.Style = (Colorizer.ColorStyle)v; })
                .PositionTopLeft(-200, 475, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 525, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new TextPicker("Score System", new string[] { "Default", "Osu", "DP" }, (int)Game.Options.Profile.ScoreSystem, v => { Game.Options.Profile.ScoreSystem = (ScoreType)v; })
                .PositionTopLeft(50, 475, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(200, 525, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new Slider("Osu OD", v => { Game.Options.Profile.OD = v; }, () => Game.Options.Profile.OD, 0, 10, 0.1f)
                .PositionTopLeft(-300, 575, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 625, AnchorType.CENTER, AnchorType.MIN)
                );
            Widgets.Add(
                new Slider("Stepmania Judge", v => { Game.Options.Profile.Judge = (int)v; }, () => Game.Options.Profile.Judge, 1, 10, 1f)
                .PositionTopLeft(50, 575, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(300, 625, AnchorType.CENTER, AnchorType.MIN)
                );
        }
    }
}
