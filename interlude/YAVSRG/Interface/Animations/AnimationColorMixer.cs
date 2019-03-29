using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace Interlude.Interface.Animations
{
    public class AnimationColorMixer : Animation
    {
        AnimationSlider R;
        AnimationSlider G;
        AnimationSlider B;
        AnimationSlider A;

        public AnimationColorMixer(Color c)
        {
            R = new AnimationSlider(c.R);
            G = new AnimationSlider(c.G);
            B = new AnimationSlider(c.B);
            A = new AnimationSlider(c.A);
        }

        public override bool Running
        {
            get
            {
                return false;
            }
        }

        public void Target(Color c)
        {
            R.Target = c.R;
            G.Target = c.G;
            B.Target = c.B;
            A.Target = c.A;
        }

        public override void Update()
        {
            R.Update(); G.Update(); B.Update(); A.Update();
        }

        public static implicit operator Color(AnimationColorMixer s)
        {
            return Color.FromArgb((int)s.A.Val, (int)s.R.Val, (int)s.G.Val, (int)s.B.Val);
        }

        public void QuickTarget(Color c)
        {
            R.Target = R.Val = c.R;
            G.Target = G.Val = c.G;
            B.Target = B.Val = c.B;
            A.Target = A.Val = c.A;
        }
    }
}
