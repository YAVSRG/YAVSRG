using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Charts.YAVSRG
{
    public class Snap : OffsetItem
    {
        public BinarySwitcher taps;
        public BinarySwitcher holds;
        public BinarySwitcher middles;
        public BinarySwitcher ends;
        public BinarySwitcher mines;

        public Snap(float offset, ushort taps, ushort holds, ushort middles, ushort ends, ushort mines)
        {
            Offset = offset;
            this.taps = new BinarySwitcher(taps);
            this.holds = new BinarySwitcher(holds);
            this.middles = new BinarySwitcher(middles);
            this.ends = new BinarySwitcher(ends);
            this.mines = new BinarySwitcher(mines);
        }

        public Snap(float offset) : this(offset, 0, 0, 0, 0, 0) { }

        public override OffsetItem Interpolate(float time)
        {
            return new Snap(time, 0, 0, (ushort)(holds.value + middles.value), 0, 0);
        }

        public Snap Mask(ushort mask)
        {
            return new Snap(Offset, (ushort)(taps.value & mask), (ushort)(holds.value & mask), (ushort)(middles.value & mask), (ushort)(ends.value & mask), (ushort)(mines.value & mask));
        }

        public bool IsEmpty()
        {
            return (taps.value | holds.value | ends.value | mines.value) == 0;
        }

        public int Count
        {
            get
            {
                return taps.Count + holds.Count + ends.Count;
            }
        }

        public BinarySwitcher Combine()
        {
            return new BinarySwitcher(taps.value | holds.value | ends.value);
        }
    }
}
