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
    class ScreenLevelSelect : Screen
    {
        private ChartDifficulty diffDisplay;
        private LevelSelector selector;

        public ScreenLevelSelect()
        {
            selector = new LevelSelector();
            selector.PositionTopLeft(0, 0, 0, 0).PositionBottomRight(0, 0, 0, 0);

            foreach (ChartLoader.ChartPack p in ChartLoader.Cache)
            {
                selector.AddPack(p);
            }

            diffDisplay = new ChartDifficulty(Game.CurrentChart);
            diffDisplay.PositionTopLeft(20, 105, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(520, 605, AnchorType.MIN, AnchorType.MIN);
            Widgets.Add(diffDisplay);
            Widgets.Add(selector);
            Widgets.Add(new Button("buttonbase", "Play", () => { Push(new ScreenPlay()); })
                .PositionTopLeft(-250,-200,AnchorType.CENTER,AnchorType.CENTER)
                .PositionBottomRight(0,-100,AnchorType.CENTER,AnchorType.CENTER)
                );
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
        }

        public override void Update()
        {
            base.Update();

            if (Input.KeyTap(OpenTK.Input.Key.Plus))
            {
                Game.Options.Profile.Rate += 0.05f;
                Game.Audio.SetRate(Game.Options.Profile.Rate);
                diffDisplay.ChangeChart(Game.CurrentChart);
            }
            if (Input.KeyTap(OpenTK.Input.Key.Minus))
            {
                Game.Options.Profile.Rate -= 0.05f;
                Game.Audio.SetRate(Game.Options.Profile.Rate);
                diffDisplay.ChangeChart(Game.CurrentChart);
            }
        }

        public override void Draw()
        {
            base.Draw();
            SpriteBatch.DrawCentredText(Utils.RoundNumber(Game.Options.Profile.Rate) + "x Audio", 40f, 0, Height-180, Color.White);
        }
    }
}
