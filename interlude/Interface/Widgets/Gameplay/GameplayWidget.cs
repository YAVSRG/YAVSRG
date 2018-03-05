using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    class GameplayWidget : Widget
    {
        protected ScoreTracker scoreTracker;

        public GameplayWidget(ScoreTracker s) : base()
        {
            scoreTracker = s;
        }
    }
}
