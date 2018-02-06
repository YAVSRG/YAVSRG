using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;
using YAVSRG.Interface;

namespace YAVSRG.Options.Tabs
{
    class GeneralTab : WidgetContainer
    {
        public GeneralTab()
        {
            Widgets.Add(new Slider("Volume", (v) => { Game.Options.General.AudioVolume = v; }, () => { return Game.Options.General.AudioVolume; }, 0, 1, 0.01f)
                .PositionTopLeft(-200, 50, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(200, 75, AnchorType.CENTER, AnchorType.MIN));
            Widgets.Add(new Slider("Offset (NYI)", (v) => { Game.Options.General.UniversalAudioOffset = v; }, () => { return Game.Options.General.UniversalAudioOffset; }, -100, 100, 1f)
                .PositionTopLeft(-200, 150, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(200, 175, AnchorType.CENTER, AnchorType.MIN));
            Widgets.Add(new TextPicker("Window Mode", new string[] { "Windowed", "Borderless", "Fullscreen" }, (int)Game.Options.General.WindowMode, (v) => { Game.Options.General.WindowMode = (General.WindowType)v; })
                .PositionTopLeft(-200, 250, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(-50, 275, AnchorType.CENTER, AnchorType.MIN));
            Widgets.Add(new TextPicker("Frame Limit", new string[] { "Unlimited", "60", "120", "180", "240" }, Game.Options.General.FrameLimiter/60, (v) => { Game.Options.General.FrameLimiter = v * 60; })
                .PositionTopLeft(50, 250, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(200, 275, AnchorType.CENTER, AnchorType.MIN));
            Widgets.Add(new Button("buttonbase", "Apply", () => { Game.Instance.ApplyWindowSettings(Game.Options.General); })
                .PositionTopLeft(-100, 450, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(100, 475, AnchorType.CENTER, AnchorType.MIN));
            Widgets.Add(new Button("buttonbase", "Open Data Folder", () => {
                System.Diagnostics.Process.Start("file://" + Content.WorkingDirectory);
            }).PositionTopLeft(0, 100, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
        }
    }
}
