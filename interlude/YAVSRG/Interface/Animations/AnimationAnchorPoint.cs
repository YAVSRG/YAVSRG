using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Animations
{
    //Handles the anchoring of a widget on a certain axis
    //Absolute position = Min + (Max-Min)*Position
    public class AnimationAnchorPoint : Animation
    {
        private AnimationSlider Position;
        private AnchorType Anchor = AnchorType.MIN;
        private float AnchorPos;

        private float GetRelativePosition(float pos, float min, float max)
        {
            return min + (max - min) * AnchorPos + pos;
            /*switch (Anchor)
            {
                case AnchorType.MAX:
                    return max - pos;
                case AnchorType.MIN:
                    return min + pos;
                case AnchorType.LERP:
                    return min + (max - min) * pos;
                case AnchorType.CENTER:
                default:
                    return (max + min) * 0.5f + pos;
            }*/
        }

        //gets position to use to place this point at desired location as if the anchor type were MIN
        private float GetStaticPosition(float target, float min, float max)
        {
            return target - (max - min) * AnchorPos;
            /*
            switch (Anchor)
            {
                case AnchorType.MAX:
                    return (max - min) - target;
                case AnchorType.MIN:
                    return target;
                case AnchorType.LERP:
                    return target / (max - min);
                case AnchorType.CENTER:
                default:
                    return target - (max - min) * 0.5f;
            }*/
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
            Anchor = type;
            switch (type)
            {
                case AnchorType.MAX:
                    Move(-pos, 1f); return;
                case AnchorType.MIN:
                    Move(pos, 0f); return;
                case AnchorType.LERP:
                    Move(0, pos); return;
                case AnchorType.CENTER:
                default:
                    Move(pos, 0.5f); return;
            }
        }

        public void Move(float pos, float anchor)
        {
            Position = new AnimationSlider(pos);
            AnchorPos = anchor;
        }

        public void Move(float pos, bool instant)
        {
            if (Anchor == AnchorType.MAX) pos *= -1;
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
