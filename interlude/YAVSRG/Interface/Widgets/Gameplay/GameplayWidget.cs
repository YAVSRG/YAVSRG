using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Gameplay;
using Interlude.Options;

namespace Interlude.Interface.Widgets.Gameplay
{
    public class GameplayWidget : Widget
    {
        protected ScoreTracker scoreTracker;

        public GameplayWidget(ScoreTracker s, WidgetPosition pos) : base()
        {
            scoreTracker = s;
            if (!pos.Enable) SetState(WidgetState.DISABLED);
            Reposition(pos.Left, pos.LeftRel, pos.Top, pos.TopRel, pos.Right, pos.RightRel, pos.Bottom, pos.BottomRel);
        }
    }
}
