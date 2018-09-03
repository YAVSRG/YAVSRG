using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
namespace YAVSRG.Interface
{
    public class Screen : Widget
    {
        public virtual void OnEnter(Screen prev) { }

        public virtual void OnExit(Screen next) { }

        public virtual void OnResize() { }

        public override Rect GetBounds()
        {
            return GetBounds(new Rect(-ScreenUtils.ScreenWidth, -ScreenUtils.ScreenHeight, ScreenUtils.ScreenWidth, ScreenUtils.ScreenHeight).ExpandY(-Game.Screens.Toolbar.Height));
        }
    }
}
