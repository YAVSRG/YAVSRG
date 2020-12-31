using System;
using Interlude.Gameplay;
using Interlude.Interface.Widgets;
using Interlude.IO;
using static Interlude.Interface.ScreenUtils;

namespace Interlude.Interface.Screens
{
    public class ScreenLevelSelect : Screen
    {
        private ChartInfoControls diffDisplay;
        private LevelSelector selector;

        public ScreenLevelSelect()
        {
            selector = new LevelSelector(this);
            AddChild(selector.Reposition(0, 0.5f, 120, 0, 0, 1, 0, 1));
            AddChild(new ChartSortingControls().Reposition(0, 0, 0, 0, 0, 1, 120, 0));
            diffDisplay = new ChartInfoControls();
            AddChild(diffDisplay.Reposition(0, 0, 0, 0, -750, 1, 0, 1));

            Reposition(-ScreenWidth, 0, 0, 0, ScreenWidth, 1, 0, 1);
            Animation.Add(new Animation()); //dummy animation ensures "expansion" effect happens during screen transitions
        }

        private void OnUpdateGroups()
        {
            selector.Refresh();
        }

        private void OnUpdateChart()
        {
            diffDisplay.ChangeChart(false);
            Game.Audio.SetRate(Game.Options.Profile.Rate);
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Move(new Rect(0, 0, 0, 0));
            Game.Gameplay.OnUpdateChart += OnUpdateChart;
            Game.Audio.OnPlaybackFinish = () => { Game.Audio.Stop(); Game.Audio.Play((long)Game.CurrentChart.Data.PreviewTime); };
            diffDisplay.ChangeChart(true);
            ChartLoader.OnRefreshGroups += OnUpdateGroups;
            Game.Screens.Toolbar.Icons.Filter(0b00011111);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Input.ChangeIM(null);
            Game.Gameplay.OnUpdateChart -= OnUpdateChart;
            ChartLoader.OnRefreshGroups -= OnUpdateGroups;
            Move(new Rect(-ScreenWidth, 0, ScreenWidth, 0));
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);

            if (Game.Options.General.Hotkeys.UpSmallRate.Tapped())
            {
                ChangeRate(0.01);
            }
            else if (Game.Options.General.Hotkeys.DownSmallRate.Tapped())
            {
                ChangeRate(-0.01);
            }
            else if (Game.Options.General.Hotkeys.UpHalfRate.Tapped())
            {
                ChangeRate(0.05);
            }
            else if (Game.Options.General.Hotkeys.DownHalfRate.Tapped())
            {
                ChangeRate(-0.05);
            }
            else if (Game.Options.General.Hotkeys.UpRate.Tapped())
            {
                ChangeRate(0.1);
            }
            else if (Game.Options.General.Hotkeys.DownRate.Tapped())
            {
                ChangeRate(-0.1);
            }
        }

        public void ChangeRate(double change)
        {
            Game.Options.Profile.Rate += change;
            Game.Options.Profile.Rate = Math.Round(Game.Options.Profile.Rate, 2, MidpointRounding.AwayFromZero);
            Game.Options.Profile.Rate = Math.Max(0.5, Math.Min(Game.Options.Profile.Rate, 3.0));
            Game.Gameplay.UpdateDifficulty();
            OnUpdateChart();
        }
    }
}
