using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Gameplay;

namespace Interlude.Interface.Widgets
{
    class DebugPanel : Widget
    {
        public DebugPanel()
        {
            AddChild(new SimpleButton("Recache Charts", () => Game.Tasks.AddTask(ChartLoader.Recache(), ChartLoader.RefreshCallback, "Recaching charts", true), () => false, null).Reposition(50, 0, 50, 0, 350, 0, 100, 0));
        }
    }
}
