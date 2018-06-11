using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;
using YAVSRG.Interface;
using YAVSRG.Interface.Dialogs;

namespace YAVSRG.Options.Panels
{
    class GeneralPanel : OptionsPanel
    {
        public GeneralPanel(InfoBox ib, LayoutPanel lp) : base(ib,"General")
        {
            AddChild(
                new TooltipContainer(
                new Slider("Volume", (v) => { Game.Options.General.AudioVolume = v; }, () => { return Game.Options.General.AudioVolume; }, 0, 1, 0.01f),
                "Global audio volume setting.\n1 is the loudest and 0 is muted.", ib)
                .PositionTopLeft(-200, 125, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(200, 150, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new Slider("Audio Offset", (v) => { Game.Options.General.UniversalAudioOffset = v; }, () => { return Game.Options.General.UniversalAudioOffset; }, -100, 100, 1f),
                "This will offset audio (in milliseconds) relative to charts and may be useful if you have consistent input latency or always hit early.\nThis number should be lower if you are hitting early and higher if you are hitting late.", ib)
                .PositionTopLeft(-200, 225, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(200, 250, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new TextPicker("Window Mode", new string[] { "Windowed", "Borderless", "Fullscreen" }, (int)Game.Options.General.WindowMode, (v) => { Game.Options.General.WindowMode = (General.WindowType)v; }),
                "This selects what kind of window the game should be.\nWindowed = Regular, resizable window\nBorderless = Maximised window with no title bar or border around it\nFullscreen = Fullscreen mode", ib)
                .PositionTopLeft(-200, 325, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 350, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new TextPicker("Frame Limit", new string[] { "Unlimited", "60", "120", "180", "240" }, Game.Options.General.FrameLimiter / 60, (v) => { Game.Options.General.FrameLimiter = v * 60; }),
                "This limits the number of frames per second that the game will run at.\nUnlimited is recommended as it gives the smoothest experience.\nUsing a frame limit can save on power consumption or strain on your GPU.", ib)
                .PositionTopLeft(50, 325, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(200, 350, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new FramedButton("buttonbase", "Apply", () => { Game.Instance.ApplyWindowSettings(Game.Options.General); }),
                "Applies the selected settings (above). Without this they won't be applied until you restart the game so you don't accidentally mess them up.", ib)
                .PositionTopLeft(-150, 450, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(150, 525, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new FramedButton("buttonbase", "Change profile", () => { Game.Screens.AddDialog(new ProfileDialog((s) => { lp.Refresh(); })); }),
                "Switch your profile to another one OR create a new profile.\n(Not fully complete)", ib)
                .PositionTopLeft(-350, 550, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 625, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new FramedButton("buttonbase", "Rename profile", () => { Game.Screens.AddDialog(new TextDialog("New Profile Name:", (s) => { Game.Options.Profile.Name = s; })); }),
                "Rename your profile to a different name.\n(Not fully complete)", ib)
                .PositionTopLeft(50, 550, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(350, 625, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new FramedButton("buttonbase", "Open Data Folder",
                () => { System.Diagnostics.Process.Start("file://" + Content.WorkingDirectory); }),
                "Opens the folder which Interlude works in. This is where you put skins, charts and other settings.", ib)
            .PositionTopLeft(-150, 650, AnchorType.CENTER, AnchorType.MIN)
            .PositionBottomRight(150, 725, AnchorType.CENTER, AnchorType.MIN));
        }
    }
}
