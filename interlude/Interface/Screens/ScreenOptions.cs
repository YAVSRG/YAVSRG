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
        private Widget tabs;

        public ScreenOptions()
        {
            var ib = new InfoBox();
            tabs = new ScrollContainer(5f, 5f, false, true);
            tabs.AddChild(new GeneralPanel(ib).PositionBottomRight(ScreenUtils.ScreenWidth * 2 - 600, 800, AnchorType.MIN, AnchorType.MIN));
            tabs.AddChild(new GameplayPanel(ib).PositionBottomRight(ScreenUtils.ScreenWidth * 2 - 600, 800, AnchorType.MIN, AnchorType.MIN));
            tabs.AddChild(new Options.Tabs.LayoutTab().PositionBottomRight(ScreenUtils.ScreenWidth * 2 - 600, 800, AnchorType.MIN, AnchorType.MIN));

            AddChild(tabs.PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(600, 100, AnchorType.MAX, AnchorType.MAX));
            AddChild(ib.PositionTopLeft(550, 50, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(50, 50, AnchorType.MAX, AnchorType.MAX));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            //tabs.Switch(0);
        }
    }
}
