using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using System.Drawing;
using YAVSRG.Interface.Widgets;
using YAVSRG.Options.Tabs;

namespace YAVSRG.Interface.Screens
{
    class ScreenOptions : Screen
    {
        private WidgetSwitcher tabs;
        LayoutTab layout;

        public ScreenOptions()
        {
            tabs = new WidgetSwitcher();
            tabs.AddChild(new GeneralTab());
            layout = new LayoutTab();
            tabs.AddChild(layout);
            tabs.AddChild(new GameplayTab());

            AddChild(tabs.PositionTopLeft(0, 140, AnchorType.MIN, AnchorType.MIN)
                .PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
            AddChild(new FramedButton("buttonbase", "General", () => { tabs.Switch(0); })
                .PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(200, 80, AnchorType.MIN, AnchorType.MIN));
            AddChild(new FramedButton("buttonbase", "Layout", () => { tabs.Switch(1); layout.Refresh(); })
                .PositionTopLeft(200, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(400, 80, AnchorType.MIN, AnchorType.MIN));
            AddChild(new FramedButton("buttonbase", "Gameplay", () => { tabs.Switch(2); })
                .PositionTopLeft(400, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(600, 80, AnchorType.MIN, AnchorType.MIN));

        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            tabs.Switch(0);
        }
    }
}
