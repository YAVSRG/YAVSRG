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

        public override Rect GetBounds()
        {
            return GetBounds(ScreenUtils.Bounds.ExpandY(-Game.Screens.Toolbar.Height));
        }
    }
}
