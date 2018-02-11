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
            tabs.Add(new GeneralTab());
            layout = new LayoutTab();
            tabs.Add(layout);
            tabs.Add(new GameplayTab());
            Widgets.Add(tabs.PositionTopLeft(0, 140, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(0, 80, AnchorType.MAX, AnchorType.MAX));
            Widgets.Add(new Button("buttonbase", "General", () => { tabs.Switch(0); })
                .PositionTopLeft(0,80,AnchorType.MIN,AnchorType.MIN).PositionBottomRight(200,140,AnchorType.MIN,AnchorType.MIN));
            Widgets.Add(new Button("buttonbase", "Layout", () => { tabs.Switch(1); layout.Refresh(); })
                .PositionTopLeft(200, 80, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(400, 140, AnchorType.MIN, AnchorType.MIN));
            Widgets.Add(new Button("buttonbase", "Gameplay", () => { tabs.Switch(2); })
                .PositionTopLeft(400, 80, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(600, 140, AnchorType.MIN, AnchorType.MIN));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Instance.Toolbar.hide = false;
        }

        public override void Draw()
        {
            base.Draw();
        }
    }
}
