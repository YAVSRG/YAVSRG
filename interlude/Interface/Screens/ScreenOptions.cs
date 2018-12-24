using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using System.Drawing;
using YAVSRG.Interface.Widgets;
using YAVSRG.Options.Panels;

namespace YAVSRG.Interface.Screens
{
    class ScreenOptions : Screen
    {
        private LayoutPanel lp;
        
        public ScreenOptions()
        {
            OnResize();
        }

        public override void OnResize()
        {
            Children.Clear();
            var ib = new InfoBox();
            FlowContainer tabs = new FlowContainer();
            lp = new LayoutPanel(ib);
            tabs.AddChild(new GeneralPanel(ib, lp).PositionBottomRight(0, 900, AnchorType.MAX, AnchorType.MIN));
            tabs.AddChild(new GameplayPanel(ib, lp).PositionBottomRight(0, 900, AnchorType.MAX, AnchorType.MIN));
            tabs.AddChild(lp.PositionBottomRight(0, 900, AnchorType.MAX, AnchorType.MIN));
            lp.Refresh();

            AddChild(tabs.PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(600, 0, AnchorType.MAX, AnchorType.MAX));
            AddChild(ib.PositionTopLeft(550, 50, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(50, 50, AnchorType.MAX, AnchorType.MAX));

            AddChild(ScrollButton("General", 0, tabs));
            AddChild(ScrollButton("Gameplay", 1, tabs));
            AddChild(ScrollButton("Layout", 2, tabs));
        }

        private Widget ScrollButton(string name, int id, FlowContainer container)
        {
            return new FramedButton(name, () => { container.ScrollTo(id); }) { Highlight = () => container.VisibleIndexBottom == id, Frame = 170, HorizontalFade = 50 }.PositionTopLeft(0, id * 60, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(250, id * 60 + 60, AnchorType.MIN, AnchorType.MIN);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Gameplay.UpdateChart(); //recolor notes based on settings if they've changed
        }
    }
}
