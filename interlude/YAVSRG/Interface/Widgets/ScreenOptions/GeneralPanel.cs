using System.Collections.Generic;
using static Interlude.Options.SettingsManager;
using Interlude.Options;


namespace Interlude.Interface.Widgets
{
    class GeneralPanel : Widget
    {
        public GeneralPanel()
        {
            AddChild(
                new TooltipContainer(
                new Slider("Volume", (v) => { general.AudioVolume = v; }, () => { return general.AudioVolume; }, 0, 1, 0.01f) { ShowAsPercentage = true },
                "Global audio volume setting.\n1 is the loudest and 0 is muted.")
                .Reposition(-200, 0.5f, 125, 0, 200, 0.5f, 150, 0));
            AddChild(
                new TooltipContainer(
                    new Slider("Audio Offset", (v) => { general.UniversalAudioOffset = v; }, () => { return general.UniversalAudioOffset; }, -100, 100, 1f),
                "This will offset audio (in milliseconds) relative to your keyboard input.\nUseful if you have consistent input latency or audio plays early (due to sound drivers).\nThis number should be lower if you are hitting early and higher if you are hitting late.")
                .Reposition(-200, 0.5f, 225, 0, 200, 0.5f, 250, 0));
            AddChild(
                new TooltipContainer(
                    new TextPicker("Window Mode", new string[] { "Windowed", "Borderless", "Fullscreen" }, (int)general.WindowMode, (v) => { general.WindowMode = (General.WindowType)v; }),
                "This selects what kind of window the game should be.\nWindowed = Regular, resizable window\nBorderless = Maximised window with no title bar or border around it\nFullscreen = Fullscreen mode")
                .Reposition(-200, 0.5f, 325, 0, 200, 0.5f, 375, 0));
            AddChild(
                new TooltipContainer(
                    new TextPicker("Frame Limit", new string[] { "Unlimited", "60", "120", "180", "240" }, general.FrameLimiter / 60, (v) => { general.FrameLimiter = v * 60; }),
                "This limits the number of frames per second that the game will run at.\nUnlimited is recommended as it gives the smoothest experience.\nUsing a frame limit can save on power consumption or strain on your GPU.")
                .Reposition(250, 0.5f, 325, 0, 600, 0.5f, 375, 0));
            List<string> res = new List<string>();
            foreach (var x in General.RESOLUTIONS)
            {
                res.Add(x.Item1.ToString() + "x" + x.Item2.ToString());
            }
            AddChild(
                new TooltipContainer(
                    new TextPicker("Screen Resolution", res.ToArray(), general.Resolution, (v) => { general.Resolution = v; }),
                "This selects the screen resolution for the game when in windowed mode.")
                .Reposition(-600, 0.5f, 325, 0, -250, 0.5f, 375, 0));
            AddChild(
                new TooltipContainer(
                    new FramedButton("Open Data Folder",
                () => { System.Diagnostics.Process.Start("file://" + System.IO.Path.GetFullPath(Game.WorkingDirectory)); }, null),
                "Opens the folder which Interlude works in. This is where you put skins, charts and other settings.")
                .Reposition(-200, 0.5f, 525, 0, 200, 0.5f, 600, 0));
        }

        public override void Dispose()
        {
            base.Dispose();
            Game.Instance.ApplyWindowSettings(general);
        }
    }
}
