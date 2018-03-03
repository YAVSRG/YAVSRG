using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Animations
{
    class AnimationTimer : Animation
    {
        public int value = 0;
        int target;

        public AnimationTimer(int final)
        {
            target = final;
        }

        public override void Update()
        {
            base.Update();
            value++;
        }

        public override bool Running
        {
            get
            {
                return value < target;
            }
        }

        public override bool DisposeMe
        {
            get
            {
                return value >= target;
            }
        }
    }
}
