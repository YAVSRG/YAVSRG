using System;

namespace Prelude.Gameplay.Charts.YAVSRG
{
    public class SVPoint : OffsetItem
    {
        public float ScrollSpeed;

        public SVPoint(float offset, float sv) : base(offset)
        {
            ScrollSpeed = sv;
        }

        public override OffsetItem Interpolate(float time)
        {
            return new SVPoint(time, ScrollSpeed);
        }
    }
}
