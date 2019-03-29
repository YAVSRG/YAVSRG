using System;

namespace Prelude.Gameplay.Charts.YAVSRG
{
    public class OffsetItem
    {
        public float Offset;

        public OffsetItem(float offset)
        {
            Offset = offset;
        }

        public virtual OffsetItem Interpolate(float time) { return null; }
    }
}
