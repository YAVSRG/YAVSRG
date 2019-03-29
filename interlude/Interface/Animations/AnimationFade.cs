using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Animations
{
    class AnimationFade : Animation
    {
        public float Val;
        public float Target;
        private float threshold;

        public AnimationFade(float start, float end, float threshold)
        {
            Val = start;
            Target = end;
            this.threshold = threshold;
        }

        public override bool DisposeMe
        {
            get
            {
                return Val > threshold;
            }
        }

        public override bool Running
        {
            get
            {
                return !DisposeMe;
            }
        }

        public override void Update()
        {
            Val = Val * 0.95f + Target * 0.05f;
        }

        public static implicit operator float(AnimationFade s)
        {
            return s.Val;
        }
    }
}
