using System.Collections.Generic;
using static Interlude.Options.Options;
using Interlude.Options;


namespace Interlude.Interface.Widgets
{
    class GeneralPanel : OptionsPanel
    {
        public GeneralPanel(InfoBox ib, LayoutPanel lp) : base(ib,"General")
        {
            AddChild(
                new TooltipContainer(
                new Slider("Volume", (v) => { general.AudioVolume = v; }, () => { return general.AudioVolume; }, 0, 1, 0.01f),
                "Global audio volume setting.\n1 is the loudest and 0 is muted.", ib)
                .TL_DeprecateMe(-200, 125, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(200, 150, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new Slider("Audio Offset", (v) => { general.UniversalAudioOffset = v; }, () => { return general.UniversalAudioOffset; }, -100, 100, 1f),
                "This will offset audio (in milliseconds) relative to charts and may be useful if you have consistent input latency or always hit early.\nThis number should be lower if you are hitting early and higher if you are hitting late.", ib)
                .TL_DeprecateMe(-200, 225, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(200, 250, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new TextPicker("Window Mode", new string[] { "Windowed", "Borderless", "Borderless 2", "Fullscreen" }, (int)general.WindowMode, (v) => { general.WindowMode = (General.WindowType)v; }),
                "This selects what kind of window the game should be.\nWindowed = Regular, resizable window\nBorderless = Maximised window with no title bar or border around it\nFullscreen = Fullscreen mode", ib)
                .TL_DeprecateMe(-75, 325, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(75, 350, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new TextPicker("Frame Limit", new string[] { "Unlimited", "60", "120", "180", "240" }, general.FrameLimiter / 60, (v) => { general.FrameLimiter = v * 60; }),
                "This limits the number of frames per second that the game will run at.\nUnlimited is recommended as it gives the smoothest experience.\nUsing a frame limit can save on power consumption or strain on your GPU.", ib)
                .TL_DeprecateMe(150, 325, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(300, 350, AnchorType.CENTER, AnchorType.MIN));
            List<string> res = new List<string>();
            foreach (var x in General.RESOLUTIONS)
            {
                res.Add(x.Item1.ToString() + "x" + x.Item2.ToString());
            }
            AddChild(
                new TooltipContainer(
                    new TextPicker("Screen Resolution", res.ToArray(), general.Resolution, (v) => { general.Resolution = v; }),
                "This selects the screen resolution for the game when in windowed mode.", ib)
                .TL_DeprecateMe(-300, 325, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(-150, 350, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new FramedButton("Apply", () => { Game.Instance.ApplyWindowSettings(general); }),
                "Applies the selected settings (above). Without this they won't be applied until you restart the game so you don't accidentally mess them up.", ib)
                .TL_DeprecateMe(-150, 450, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(150, 525, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                    new FramedButton("Open Data Folder",
                () => { System.Diagnostics.Process.Start("file://" + System.IO.Path.GetFullPath(Game.WorkingDirectory)); }),
                "Opens the folder which Interlude works in. This is where you put skins, charts and other settings.", ib)
            .TL_DeprecateMe(-150, 650, AnchorType.CENTER, AnchorType.MIN)
            .BR_DeprecateMe(150, 725, AnchorType.CENTER, AnchorType.MIN));
        }
    }
}
