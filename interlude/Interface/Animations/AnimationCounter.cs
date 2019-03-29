using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Animations
{
    public class AnimationCounter : Animation
    {
        public int value = 0;
        public int cycles = 0;
        int target;
        bool loop;

        public AnimationCounter(int final, bool loop)
        {
            target = final;
            this.loop = loop;
        }

        public override void Update()
        {
            base.Update();
            value++;
            if (loop && value == target) { value = 0; cycles++; }
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
