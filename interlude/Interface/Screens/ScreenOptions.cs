using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using static YAVSRG.Interface.ScreenUtils;
using System.Drawing;
using YAVSRG.Interface.Widgets;
using YAVSRG.Options.Tabs;

namespace YAVSRG.Interface.Screens
{
    class ScreenOptions : Screen
    {
        private WidgetSwitcher tabs;

        public ScreenOptions()
        {
            tabs = new WidgetSwitcher();
            tabs.Add(new GeneralTab());
            tabs.Add(new GameplayTab());
            Widgets.Add(tabs);
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
