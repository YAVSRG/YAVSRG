using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static YAVSRG.Interface.ScreenUtils;
using System.Drawing;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Screens
{
    public class ScreenLevelSelect : Screen
    {
        private ChartDifficulty diffDisplay;
        private LevelSelector selector;

        public ScreenLevelSelect()
        {
            selector = new LevelSelector(this);
            selector.PositionTopLeft(0, 0, AnchorType.MIN,AnchorType.MIN).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX);

            foreach (ChartLoader.ChartPack p in ChartLoader.Cache)
            {
                selector.AddPack(p,0);
            }

            diffDisplay = new ChartDifficulty(Game.CurrentChart);
            diffDisplay.PositionTopLeft(20, 105, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(520, 605, AnchorType.MIN, AnchorType.MIN);
            Widgets.Add(diffDisplay);
            Widgets.Add(selector);
            Widgets.Add(new FramedButton("buttonbase", "Play", () => { Push(new ScreenPlay()); })
                .PositionTopLeft(-250,-200,AnchorType.CENTER,AnchorType.CENTER)
                .PositionBottomRight(0,-100,AnchorType.CENTER,AnchorType.CENTER)
                );
        }

        public void OnChangeChart()
        {
            diffDisplay.ChangeChart(Game.CurrentChart);
            Game.Audio.SetRate(Game.Options.Profile.Rate);
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
        }

        public override void Update()
        {
            base.Update();

            double ratestep = Input.KeyPress(OpenTK.Input.Key.ControlLeft) ? 0.2d : 0.05d;
            if (Input.KeyTap(OpenTK.Input.Key.Plus))
            {
                ChangeRate(ratestep);
            }
            else if (Input.KeyTap(OpenTK.Input.Key.Minus))
            {
                ChangeRate(-ratestep);
            }
        }

        public void ChangeRate(double change)
        {
            Game.Options.Profile.Rate += change;
            Game.Options.Profile.Rate = Math.Round(Game.Options.Profile.Rate, 2, MidpointRounding.AwayFromZero);
            Game.Options.Profile.Rate = Math.Max(0.5, Math.Min(Game.Options.Profile.Rate,3.0));
            Game.Audio.SetRate(Game.Options.Profile.Rate);
            diffDisplay.ChangeChart(Game.CurrentChart);
        }

        public override void Draw()
        {
            base.Draw();
            SpriteBatch.DrawCentredText(Utils.RoundNumber(Game.Options.Profile.Rate) + "x Audio", 40f, 0, Height-180, Color.White);
        }
    }
}
