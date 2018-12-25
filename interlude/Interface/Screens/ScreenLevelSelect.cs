using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Charts;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Screens
{
    public class ScreenLevelSelect : Screen
    {
        private ChartInfoControls diffDisplay;
        private LevelSelector selector;

        public ScreenLevelSelect()
        {
            selector = new LevelSelector(this);
            AddChild(selector.PositionTopLeft(0, 120, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
            AddChild(new ChartSortingControls().PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(0, 120, AnchorType.MAX, AnchorType.MIN));
            diffDisplay = new ChartInfoControls();
            AddChild(diffDisplay.PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(750, 0, AnchorType.MAX, AnchorType.MAX));

            PositionTopLeft(-ScreenWidth, 0, AnchorType.MIN, AnchorType.MIN);
            PositionBottomRight(-ScreenWidth, 0, AnchorType.MAX, AnchorType.MAX);
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
            Move(new Rect(0, 0, 0, 0), false);
            Game.Gameplay.OnUpdateChart += OnUpdateChart;
            Game.Audio.OnPlaybackFinish = () => { Game.Audio.Stop(); Game.Audio.Play((long)Game.CurrentChart.Data.PreviewTime); };
            diffDisplay.ChangeChart(true);
            ChartLoader.OnRefreshGroups += OnUpdateGroups;
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Input.ChangeIM(null);
            Game.Gameplay.OnUpdateChart -= OnUpdateChart;
            ChartLoader.OnRefreshGroups -= OnUpdateGroups;
            Move(new Rect(-ScreenWidth, 0, -ScreenWidth, 0), false);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);

            double ratestep = Input.KeyPress(OpenTK.Input.Key.ControlLeft) ? 0.05d : Input.KeyPress(OpenTK.Input.Key.ShiftLeft) ? 0.01d : 0.1d;
            if (Input.KeyTap(Game.Options.General.Binds.UpRate))
            {
                ChangeRate(ratestep);
            }
            else if (Input.KeyTap(Game.Options.General.Binds.DownRate))
            {
                ChangeRate(-ratestep);
            }
            else if (Input.KeyPress(OpenTK.Input.Key.ControlLeft) && Input.KeyTap(OpenTK.Input.Key.R)) //temp
            {
                Game.Tasks.AddTask(ChartLoader.Recache(), ChartLoader.RefreshCallback, "Recaching charts", true);
            }
            else if (Input.KeyTap(Game.Options.General.Binds.Select))
            {
                Game.Gameplay.PlaySelectedChart();
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
