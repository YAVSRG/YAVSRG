using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface
{
    public class SlidingEffect : Animation
    {
        public float Val;
        public float Target;

        public SlidingEffect(float p)
        {
            Val = p;
            Target = p;
        }

        public override bool Running
        {
            get
            {
                return base.Running;
            }
        }

        public override void Update()
        {
            Val = Val * 0.95f + Target * 0.05f;
        }

        public static implicit operator float (SlidingEffect s)
        {
            return s.Val;
        }
    }
}
