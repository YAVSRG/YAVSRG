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
        }

        //gets position to use to place this point at desired location as if the anchor type were MIN
        private float GetStaticPosition(float target, float min, float max)
        {
            return target - (max - min) * AnchorPos;
        }

        public AnimationAnchorPoint(float Position, float Anchor)
        {
            this.Position = new AnimationSlider(Position);
            AnchorPos = Anchor;
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

        //Returns the position of this anchor point relative to the minimum and maximum X or Y values as bounds
        //Projected = true will retrieve where the widget will be in the immediate future as animation completes
        //By default this returns the actual bounds of the widget which are different if it is moving/animated
        public float GetPosition(float Min, float Max, bool Projected = false)
        {
            return GetRelativePosition(Projected ? Position.Target : Position.Val, Min, Max);
        }

        //Instantly repositions the anchor point to a new arrangement
        public void Reposition(float NewPosition, float Anchor)
        {
            Position.NewPosition = NewPosition;
            AnchorPos = Anchor;
        }

        //Instantly repositions the anchor point to a new arrangement.
        //Old anchor position is preserved.
        public void Reposition(float NewPosition)
        {
            if (Anchor == AnchorType.MAX) NewPosition *= -1;
            Position.NewPosition = NewPosition;
        }

        //Instantly repositions the anchor point to a new arrangement.
        //The new position is relative to the bounds and the old anchor position is preserved.
        public void RepositionRelative(float NewPosition, float Min, float Max)
        {
            Position.NewPosition = GetStaticPosition(NewPosition, Min, Max);
        }

        //Moves the anchor point smoothly to the new location
        //The new position is relative to the anchor position
        public void Move(float NewPosition)
        {
            if (Anchor == AnchorType.MAX) NewPosition *= -1;
            Position.Target = NewPosition;
        }

        //Moves the anchor point smoothly to the new location.
        //The new position is relative to the bounds rather than the anchor position
        public void MoveRelative(float NewPosition, float Min, float Max)
        {
            Position.Target = GetStaticPosition(NewPosition, Min, Max);
        }
    }
}
