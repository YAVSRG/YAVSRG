using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Animations
{
    public class AnimationSlider : Animation
    {
        public float Val;
        public float Target;
        public virtual float NewPosition
        {
            set { Val = Target = value; }
        }

        public AnimationSlider(float p)
        {
            Val = p;
            Target = p;
        }

        public override bool Running
        {
            get
            {
                return false;
            }
        }

        public override void Update()
        {
            Val = Val * 0.95f + Target * 0.05f;
        }

        public static implicit operator float (AnimationSlider s)
        {
            return s.Val;
        }
    }
}
