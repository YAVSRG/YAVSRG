using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using static YAVSRG.Interface.ScreenUtils;
using System.Drawing;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Screens
{
    class ScreenOptions : Screen
    {
        private WidgetSwitcher tabs;

        public ScreenOptions(PlayingChart data)
        {
            tabs = new WidgetSwitcher();
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Toolbar.hide = false;
        }

        public override void Draw()
        {
            base.Draw();
        }
    }
}
