using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Animations
{
    public class AnchorPoint : Animation
    {
        private AnimationSlider _X;
        private AnimationSlider _Y;
        private AnchorType XRel;
        private AnchorType YRel;

        public AnchorPoint(float x, float y, AnchorType xa, AnchorType ya)
        {
            Reposition(x, y, xa, ya);
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
            _X.Update();
            _Y.Update();
        }

        public void Reposition(float x, float y, AnchorType xa, AnchorType ya)
        {
            _X = new AnimationSlider(x);
            _Y = new AnimationSlider(y);
            XRel = xa;
            YRel = ya;
        }

        public void MoveTarget(float x, float y)
        {
            _X.Target += x;
            _Y.Target += y;
        }

        public void Target(float x, float y)
        {
            _X.Target = x;
            _Y.Target = y;
        }

        public void Position(float x, float y)
        {
            _X.Val = x;
            _Y.Val = y;
        }

        public float X(float min, float max)
        {
            switch (XRel)
            {
                case (AnchorType.CENTER):
                    return (max+min)*0.5f+_X;
                case (AnchorType.MAX):
                    return max - _X;
                case (AnchorType.MIN):
                    return min + _X;
            }
            return _X;
        }

        public float Y(float min, float max)
        {
            switch (YRel)
            {
                case (AnchorType.CENTER):
                    return _Y;
                case (AnchorType.MAX):
                    return max - _Y;
                case (AnchorType.MIN):
                    return min + _Y;
            }
            return _Y;
        }

        public float AbsX
        {
            get
            {
                return _X;
            }
        }

        public float AbsY
        {
            get
            {
                return _Y;
            }
        }

        public float TargetX
        {
            get
            {
                return _X.Target;
            }
        }

        public float TargetY
        {
            get
            {
                return _Y.Target;
            }
        }
    }
}
