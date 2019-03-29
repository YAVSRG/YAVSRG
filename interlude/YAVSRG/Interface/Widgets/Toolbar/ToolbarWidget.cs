using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Widgets.Toolbar
{
    public class ToolbarWidget : Widget
    {
        public virtual void OnToolbarCollapse()
        {

        }

        protected bool ToolbarCollapsed { get { return Game.Screens.Toolbar.State != WidgetState.ACTIVE; } }
    }
}
