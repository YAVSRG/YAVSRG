using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Beatmap
{
    public class Snap : OffsetItem
    {
        public class BinarySwitcher
        {
            public int value;

            public BinarySwitcher(int v)
            {
                value = v;
            }

            public bool GetColumn(int i)
            {
                return (value & (1 << i)) > 0;
            }

            public void SetColumn(int i)
            {
                value |= (1<<i);
            }

            public void RemoveColumn(int i)
            {
                if (GetColumn(i))
                {
                    value -= (1<<i);
                }
            }

            public void ToggleColumn(int i)
            {
                value ^= (1<<i);
            }

            public IEnumerable<int> GetColumns()
            {
                int temp = value;
                int i = 0;
                while (temp > 0)
                {
                    if (temp % 2 == 1)
                    {
                        yield return i;
                    }
                    temp >>= 1;
                    i += 1;
                }
            }

            public int Count
            {
                get { int i = 0; foreach (int k in GetColumns()) { i++; } return i; }
            }
        }

        public BinarySwitcher taps;
        public BinarySwitcher holds;
        public BinarySwitcher middles;
        public BinarySwitcher ends;

        public int[] colors;

        public Snap(float offset, int taps, int holds, int middles, int ends)
        {
            Offset = offset;
            this.taps = new BinarySwitcher(taps);
            this.holds = new BinarySwitcher(holds);
            this.middles = new BinarySwitcher(middles);
            this.ends = new BinarySwitcher(ends);

            colors = new int[7];
        }

        public Snap(float offset) : this(offset, 0, 0, 0, 0) { }

        public Snap Mask(int mask)
        {
            return new Snap(Offset, taps.value & mask, holds.value & mask, middles.value & mask, ends.value & mask);
        }

        public bool IsEmpty()
        {
            return (taps.value | holds.value | middles.value | ends.value) == 0;
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
