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
            selector.PositionTopLeft(0, 0, AnchorType.CENTER,AnchorType.MIN).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX);

            diffDisplay = new ChartDifficulty();
            diffDisplay.PositionTopLeft(100, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(600, 0, AnchorType.MIN, AnchorType.MAX);
            AddChild(diffDisplay);
            AddChild(selector);
            AddChild(new FramedButton("buttonbase", "Play", () => { Game.Screens.AddScreen(new ScreenPlay()); })
                .PositionTopLeft(250,100,AnchorType.MIN,AnchorType.CENTER)
                .PositionBottomRight(450,200,AnchorType.MIN,AnchorType.CENTER));
            //temp mod selection menu
            FlowContainer f = new FlowContainer(20,20) { };
            foreach (Gameplay.Mods.Mod m in Game.Gameplay.mods)
            {
                f.AddChild(new BoolPicker(m.GetName(), m.Enable, ModSelectClosure(m)).PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(120, 40, AnchorType.MIN, AnchorType.MIN));
            }
            AddChild(f.PositionTopLeft(-100,0,AnchorType.CENTER,AnchorType.CENTER).PositionBottomRight(100,0,AnchorType.CENTER,AnchorType.MAX));
            //
            PositionTopLeft(-ScreenWidth, 0, AnchorType.MIN, AnchorType.MIN);
            PositionBottomRight(-ScreenWidth, 0, AnchorType.MAX, AnchorType.MAX);
            Animation.Add(new Animation()); //dummy animation ensures "expansion" effect happens during screen transitions
        }

        private Action<bool> ModSelectClosure(Gameplay.Mods.Mod m)
        {
            return (b) => { m.Enable = b; };
        }

        public void OnChangeChart()
        {
            diffDisplay.ChangeChart();
            Game.Audio.SetRate(Game.Options.Profile.Rate);
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            A.Target(0, 0);
            B.Target(0, 0);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            A.Target(-ScreenWidth, 0);
            B.Target(-ScreenWidth, 0);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);

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
            diffDisplay.ChangeChart();
        }
    }
}
