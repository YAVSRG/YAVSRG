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

        public void Reposition(float x, float y, AnchorType xa, AnchorType ya) //totally resets the anchor point to new positioning
        {
            _X = new AnimationSlider(x);
            _Y = new AnimationSlider(y);
            XRel = xa;
            YRel = ya;
        }

        private float FunctionName(float target, float min, float max, AnchorType rel)
        {
            switch (rel)
            {
                case (AnchorType.CENTER):
                    return target - (max - min) * 0.5f; // ??
                case (AnchorType.MAX):
                    return (max - min) - target;
                case (AnchorType.MIN):
                    return target;
                case (AnchorType.LERP):
                    return target / (max - min);
                default:
                    return 0f;
            }
        }

        public void Target(float x, float y, Rect bounds) //targets a new location smoothly
        {
            _X.Target = FunctionName(x, bounds.Left, bounds.Right, XRel);
            _Y.Target = FunctionName(y, bounds.Top, bounds.Bottom, YRel);
        }

        public void Target(float x, float y) //targets a new location smoothly, no respect to anchor types or bounds
        {
            _X.Target = x;
            _Y.Target = y;
        }

        public void Position(float x, float y, Rect bounds) //moves instantly to a new location
        {
            _X.Val = FunctionName(x, bounds.Left, bounds.Right, XRel);
            _Y.Val = FunctionName(y, bounds.Top, bounds.Bottom, YRel);
        }

        public float X(float min, float max)
        {
            switch (XRel)
            {
                case (AnchorType.CENTER):
                    return (max + min) * 0.5f + _X;
                case (AnchorType.MAX):
                    return max - _X;
                case (AnchorType.MIN):
                    return min + _X;
                case (AnchorType.LERP):
                    return min + (max - min) * _X;
            }
            return _X;
        }

        public float Y(float min, float max)
        {
            switch (YRel)
            {
                case (AnchorType.CENTER):
                    return (max + min) * 0.5f + _Y;
                case (AnchorType.MAX):
                    return max - _Y;
                case (AnchorType.MIN):
                    return min + _Y;
                case (AnchorType.LERP):
                    return min + (max - min) * _Y;
            }
            return _Y;
        }
        
        public float AbsoluteX
        {
            get
            {
                return _X;
            }
        }

        public float AbsoluteY
        {
            get
            {
                return _Y;
            }
        }

        public float AbsoluteTargetX
        {
            get
            {
                return _X.Target;
            }
        }

        public float AbsoluteTargetY
        {
            get
            {
                return _Y.Target;
            }
        }
    }
}
