using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Gameplay.Collections
{
    class GoalData : PlaylistData
    {
        public enum GoalType
        {
            LifeClear,
            Accuracy,
            Grade
        }

        public GoalType Type;
        public float Target;
    }
}
