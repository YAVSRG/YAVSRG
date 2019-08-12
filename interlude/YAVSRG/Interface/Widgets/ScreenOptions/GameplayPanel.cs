using static Prelude.Gameplay.ScoreMetrics.ScoreSystem;
using Prelude.Gameplay.ScoreMetrics.Accuracy;
using Prelude.Utilities;
using Interlude.Interface.Dialogs;
using Interlude.Options;

namespace Interlude.Interface.Widgets
{
    class GameplayPanel : Widget
    {
        public GameplayPanel()
        {
            AddChild(
                new TooltipContainer(
                new Slider("Scroll speed", v => { Game.Options.Profile.ScrollSpeed = v; }, () => Game.Options.Profile.ScrollSpeed, 1, 4, 0.01f),
                "Increasing this will increase the speed at which notes scroll across the screen.\nA scroll speed of 1 means that 1 pixel corresponds to 1ms, or 1000 pixels corresponds to 1 second.")
                .TL_DeprecateMe(-300, 75, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(-50, 100, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("Hit Position", v => { Game.Options.Profile.HitPosition = (int)v; }, () => Game.Options.Profile.HitPosition, -100, 400, 1),
                "This moves the position that notes need to be on your screen when you hit them (by moving the receptors).\nBigger numbers move the receptors closer to the centre of the screen.")
                .TL_DeprecateMe(50, 75, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(300, 100, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("3D Perspective", v => { Game.Options.Profile.PerspectiveTilt = v; }, () => Game.Options.Profile.PerspectiveTilt, -0.75f, 0.75f, 0.01f),
                "This tilts the playfield to give a 3D perspective.\nNegative numbers tilt away from receptors and positive numbers tilt towards receptors.")
                .TL_DeprecateMe(-300, 175, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(-50, 200, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("Background Dim", v => { Game.Options.Profile.BackgroundDim = v; }, () => Game.Options.Profile.BackgroundDim, 0, 1, 0.01f),
                "This controls the dimming effect of the background image when you are playing.\n0 = No dimming\n1 = Completely black")
                .TL_DeprecateMe(50, 175, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(300, 200, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("Upwards Screencover", v => { Game.Options.Profile.ScreenCoverUp = v; }, () => Game.Options.Profile.ScreenCoverUp, 0, 1, 0.01f),
                "This is the proportion of the screen covered by a screen cover. Some players may find it easier to read notes when using a screencover.\nThis screencover is for the receptor end of the screen.")
                .TL_DeprecateMe(-300, 275, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(-50, 300, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("Downwards Screencover", v => { Game.Options.Profile.ScreenCoverDown = v; }, () => Game.Options.Profile.ScreenCoverDown, 0, 1, 0.01f),
                "This is the proportion of the screen covered by a screen cover. Some players may find it easier to read notes when using a screencover.\nThis screencover is for the non-receptor end of the screen.")
                .TL_DeprecateMe(50, 275, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(300, 300, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new BoolPicker("Upscroll", Game.Options.Profile.Upscroll, v => { Game.Options.Profile.Upscroll = v; }),
                "Turn this on if you want the notes to scroll upwards instead of downwards.")
                .TL_DeprecateMe(-200, 375, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(-50, 425, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new BoolPicker("Enable Hitlights", Game.Options.Profile.HitLighting, v => { Game.Options.Profile.HitLighting = v; }),
                "Turn this on to enable columns lighting up when you press a key.")
                .TL_DeprecateMe(50, 375, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(300, 425, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new TextPicker("Note Color Style", new string[] { "DDR", "Column", "Chord", "Jackhammer" }, (int)Game.Options.Profile.ColorStyle.Style, v => { Game.Options.Profile.ColorStyle.Style = (Colorizer.ColorStyle)v; }),
                "This is the color scheme for notes when playing.\nDDR = Color notes by musical rhythm i.e make every other beat red and the remaining beats green\nColumn = Each column has a specific color for its notes\nChord = Color chords of notes by the number of notes in the chord")
                .TL_DeprecateMe(-200, 475, AnchorType.CENTER, AnchorType.MIN)
                .BR_DeprecateMe(-50, 525, AnchorType.CENTER, AnchorType.MIN));
            ObjectSelector<ScoreSystemData> o = null; //suck my nuts compiler
            o = new ObjectSelector<ScoreSystemData>(
            Game.Options.Profile.ScoreSystems,
            (t) => t.Instantiate().Name,
            () => Game.Options.Profile.SelectedScoreSystem,
            (i) => Game.Options.Profile.SelectedScoreSystem = i,
            () => Game.Options.Profile.ScoreSystems.Add(new ScoreSystemData(ScoreType.Default, new DataGroup())),
            () => { Game.Options.Profile.ScoreSystems.RemoveAt(Game.Options.Profile.SelectedScoreSystem); Game.Options.Profile.SelectedScoreSystem -= 1; },
            () =>
            {
                Game.Screens.AddDialog(new ConfigDialog((s) => o.Refresh(), "Configure score system",
                    Game.Options.Profile.ScoreSystems[Game.Options.Profile.SelectedScoreSystem].Data,
                    Game.Options.Profile.ScoreSystems[Game.Options.Profile.SelectedScoreSystem].Type == ScoreType.Osu ? typeof(OsuMania) : typeof(DancePoints)));
            }, () => { Game.Options.Profile.ScoreSystems[Game.Options.Profile.SelectedScoreSystem].Type = (ScoreType)((int)(Game.Options.Profile.ScoreSystems[Game.Options.Profile.SelectedScoreSystem].Type + 1) % 5); }
            );
            AddChild(o.Reposition(50, 0.5f, 475, 0, -50, 1, 875, 0));
        }
    }
}
