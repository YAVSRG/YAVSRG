using static Prelude.Gameplay.ScoreMetrics.ScoreSystem;
using Prelude.Gameplay.ScoreMetrics.Accuracy;
using Prelude.Utilities;
using Interlude.Interface.Dialogs;
using Interlude.Gameplay;

namespace Interlude.Interface.Widgets
{
    class GameplayPanel : Widget
    {
        public GameplayPanel()
        {
            AddChild(
                new TooltipContainer(
                new Slider("Scroll speed", v => { Game.Options.Profile.ScrollSpeed = v; }, () => Game.Options.Profile.ScrollSpeed, 1, 4, 0.01f) { ShowAsPercentage = true },
                "Increasing this will increase the speed at which notes scroll across the screen.\nA scroll speed of 1 means that 1 pixel corresponds to 1ms, or 1000 pixels corresponds to 1 second.")
                .Reposition(-300, 0.5f, 75, 0, -50, 0.5f, 100, 0));
            AddChild(
                new TooltipContainer(
                new Slider("Hit Position", v => { Game.Options.Profile.HitPosition = (int)v; }, () => Game.Options.Profile.HitPosition, -100, 400, 1),
                "This moves the position that notes need to be on your screen when you hit them (by moving the receptors).\nBigger numbers move the receptors closer to the centre of the screen.")
                .Reposition(50, 0.5f, 75, 0, 300, 0.5f, 100, 0));
            AddChild(
                new TooltipContainer(
                new Slider("3D Perspective", v => { Game.Options.Profile.PerspectiveTilt = v; }, () => Game.Options.Profile.PerspectiveTilt, -0.75f, 0.75f, 0.01f) { ShowAsPercentage = true },
                "This tilts the playfield to give a 3D perspective.\nNegative numbers tilt away from receptors and positive numbers tilt towards receptors.")
                .Reposition(-300, 0.5f, 175, 0, -50, 0.5f, 200, 0));
            AddChild(
                new TooltipContainer(
                new Slider("Background Dim", v => { Game.Options.Profile.BackgroundDim = v; }, () => Game.Options.Profile.BackgroundDim, 0, 1, 0.01f) { ShowAsPercentage = true },
                "This controls the dimming effect of the background image when you are playing.\n0 = No dimming\n1 = Completely black")
                .Reposition(50, 0.5f, 175, 0, 300, 0.5f, 200, 0));
            AddChild(
                new TooltipContainer(
                new Slider("Upwards Screencover", v => { Game.Options.Profile.ScreenCoverUp = v; }, () => Game.Options.Profile.ScreenCoverUp, 0, 1, 0.01f) { ShowAsPercentage = true },
                "This is the proportion of the screen covered by a screen cover. Some players may find it easier to read notes when using a screencover.\nThis screencover is for the receptor end of the screen.")
                .Reposition(-300, 0.5f, 275, 0, -50, 0.5f, 300, 0));
            AddChild(
                new TooltipContainer(
                new Slider("Downwards Screencover", v => { Game.Options.Profile.ScreenCoverDown = v; }, () => Game.Options.Profile.ScreenCoverDown, 0, 1, 0.01f) { ShowAsPercentage = true },
                "This is the proportion of the screen covered by a screen cover. Some players may find it easier to read notes when using a screencover.\nThis screencover is for the non-receptor end of the screen.")
                .Reposition(50, 0.5f, 275, 0, 300, 0.5f, 300, 0));
            AddChild(
                new TooltipContainer(
                new BoolPicker("Upscroll", Game.Options.Profile.Upscroll, v => { Game.Options.Profile.Upscroll = v; }),
                "Turn this on if you want the notes to scroll upwards instead of downwards.")
                .Reposition(-300, 0.5f, 375, 0, -50, 0.5f, 425, 0));
            AddChild(
                new TooltipContainer(
                new BoolPicker("Enable Hitlights", Game.Options.Profile.HitLighting, v => { Game.Options.Profile.HitLighting = v; }),
                "Turn this on to enable columns lighting up when you press a key.")
                .Reposition(-300, 0.5f, 475, 0, -50, 0.5f, 525, 0));
            AddChild(
                new TooltipContainer(
                Selector.FromEnum<HPFailType>("HP Fail Type", Game.Options.Profile, "HPFailType"),
                "TODO: SET DESCRIPTION")
                .Reposition(-300, 0.5f, 575, 0, -50, 0.5f, 625, 0));

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
            AddChild(o.Reposition(50, 0.5f, 375, 0, -50, 1, 775, 0));
        }
    }
}
