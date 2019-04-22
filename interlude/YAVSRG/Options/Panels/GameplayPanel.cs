using Interlude.Interface.Widgets;
using Interlude.Interface;
using static Prelude.Gameplay.Watchers.ScoreSystem;
using Interlude.Interface.Dialogs;

namespace Interlude.Options.Panels
{
    class GameplayPanel : OptionsPanel
    {
        public GameplayPanel(InfoBox ib, LayoutPanel lp) : base(ib, "Gameplay")
        {
            AddChild(
                new TooltipContainer(
                new Slider("Scroll speed", v => { Game.Options.Profile.ScrollSpeed = v; }, () => Game.Options.Profile.ScrollSpeed, 1, 4, 0.01f),
                "Increasing this will increase the speed at which notes scroll across the screen.\nA scroll speed of 1 means that 1 pixel corresponds to 1ms, or 1000 pixels corresponds to 1 second.", ib)
                .PositionTopLeft(-300, 100, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 150, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("Hit Position", v => { Game.Options.Profile.HitPosition = (int)v; }, () => Game.Options.Profile.HitPosition, -100, 400, 1),
                "This moves the position that notes need to be on your screen when you hit them (by moving the receptors).\nBigger numbers move the receptors closer to the centre of the screen.", ib)
                .PositionTopLeft(50, 100, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(300, 150, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("3D Perspective", v => { Game.Options.Profile.PerspectiveTilt = v; }, () => Game.Options.Profile.PerspectiveTilt, -0.75f, 0.75f, 0.01f),
                "This tilts the playfield to give a 3D perspective.\nNegative numbers tilt away from receptors and positive numbers tilt towards receptors.", ib)
                .PositionTopLeft(-300, 175, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 225, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("Background Dim", v => { Game.Options.Profile.BackgroundDim = v; }, () => Game.Options.Profile.BackgroundDim, 0, 1, 0.01f),
                "This controls the dimming effect of the background image when you are playing.\n0 = No dimming\n1 = Completely black", ib)
                .PositionTopLeft(50, 175, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(300, 225, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("Upwards Screencover", v => { Game.Options.Profile.ScreenCoverUp = v; }, () => Game.Options.Profile.ScreenCoverUp, 0, 1, 0.01f),
                "This is the proportion of the screen covered by a screen cover. Some players may find it easier to read notes when using a screencover.\nThis screencover is for the receptor end of the screen.", ib)
                .PositionTopLeft(-300, 275, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 325, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new Slider("Downwards Screencover", v => { Game.Options.Profile.ScreenCoverDown = v; }, () => Game.Options.Profile.ScreenCoverDown, 0, 1, 0.01f),
                "This is the proportion of the screen covered by a screen cover. Some players may find it easier to read notes when using a screencover.\nThis screencover is for the non-receptor end of the screen.", ib)
                .PositionTopLeft(50, 275, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(300, 325, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new BoolPicker("Upscroll", Game.Options.Profile.Upscroll, v => { Game.Options.Profile.Upscroll = v; }),
                "Turn this on if you want the notes to scroll upwards instead of downwards.", ib)
                .PositionTopLeft(-200, 375, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 425, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new BoolPicker("Arrows for 4k", Game.Options.Profile.UseArrowsFor4k, v => { Game.Options.Profile.UseArrowsFor4k = v; lp.Refresh(); }),
                "Turn this on if you want to use arrow textures when playing with four keys. This uses the arrow texture provided by the skin, which will be rotated depending on the column.", ib)
                .PositionTopLeft(50, 375, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(200, 425, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new TextPicker("Note Color Style", new string[] { "DDR", "Column", "Chord", "Jackhammer" }, (int)Game.Options.Profile.ColorStyle.Style, v => { Game.Options.Profile.ColorStyle.Style = (Colorizer.ColorStyle)v; lp.Refresh(); }),
                "This is the color scheme for notes when playing.\nDDR = Color notes by musical rhythm i.e make every other beat red and the remaining beats green\nColumn = Each column has a specific color for its notes\nChord = Color chords of notes by the number of notes in the chord", ib)
                .PositionTopLeft(-200, 475, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(-50, 525, AnchorType.CENTER, AnchorType.MIN));
            AddChild(
                new TooltipContainer(
                new TextPicker("Score System", new string[] { "Default", "Osu", "DP", "Wife", "SC+" }, (int)Game.Options.Profile.ScoreSystems[0].Type, v => { Game.Options.Profile.ScoreSystems[0] = new AccuracySystemSelector.AccuracySystem((ScoreType)v, Game.Options.Profile.ScoreSystems[0].Data); Game.Screens.AddDialog(new ConfigDialog((s) => { }, "Configure score system", Game.Options.Profile.ScoreSystems[0].Data, Game.Options.Profile.ScoreSystems[0].Type == ScoreType.Osu ? typeof(Prelude.Gameplay.Watchers.Scoring.OsuMania) : typeof(Prelude.Gameplay.Watchers.Scoring.DancePoints))); }),
                "This is the accuracy measurement system to use when playing.\nOsu = osu!mania's accuracy system\nWife = Etterna's accuracy system", ib)
                .PositionTopLeft(50, 475, AnchorType.CENTER, AnchorType.MIN)
                .PositionBottomRight(200, 525, AnchorType.CENTER, AnchorType.MIN));
        }
    }
}
