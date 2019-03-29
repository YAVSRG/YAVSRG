using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Animations
{
    public class AnimationAnchorPoint : Animation
    {
        private AnimationSlider Position;
        private AnchorType Anchor;

        private float GetRelativePosition(float pos, float min, float max)
        {
            switch (Anchor)
            {
                case (AnchorType.MAX):
                    return max - Position;
                case (AnchorType.MIN):
                    return min + Position;
                case (AnchorType.LERP):
                    return min + (max - min) * Position;
                case (AnchorType.CENTER):
                default:
                    return (max + min) * 0.5f + Position;
            }
        }

        //gets position to use to place this point at desired location as if the anchor type were MIN
        private float GetStaticPosition(float target, float min, float max)
        {
            switch (Anchor)
            {
                case (AnchorType.MAX):
                    return (max - min) - target;
                case (AnchorType.MIN):
                    return target;
                case (AnchorType.LERP):
                    return target / (max - min);
                case (AnchorType.CENTER):
                default:
                    return target - (max - min) * 0.5f;
            }
        }

        public AnimationAnchorPoint(float pos, AnchorType type)
        {
            Move(pos, type);
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
            Position.Update();
        }

        public void Move(float pos, AnchorType type)
        {
            Position = new AnimationSlider(pos);
            Anchor = type;
        }

        public void Move(float pos, bool instant)
        {
            if (instant) Position.Val = Position.Target = pos;
            else Position.Target = pos;
        }

        public void Move(float pos, bool instant, float min, float max)
        {
            if (instant) Position.Val = Position.Target = GetStaticPosition(pos, min, max);
            else Position.Target = GetStaticPosition(pos, min, max);
        }

        public float RelativePos(float min, float max, bool target)
        {
            return GetRelativePosition(target ? Position.Target : Position.Val, min, max);
        }

        public float StaticPos(bool target)
        {
            return target ? Position.Target : Position.Val;
        }
    }
}
