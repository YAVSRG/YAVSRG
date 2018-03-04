using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Animations
{
    class AnimationAction : Animation
    {
        bool complete;
        Action action;

        public AnimationAction(Action a)
        {
            action = a;
        }

        public override bool DisposeMe
        {
            get
            {
                return complete;
            }
        }

        public override bool Running
        {
            get
            {
                return !complete;
            }
        }

        public override void Update()
        {
            action(); complete = true;
        }
    }
}
