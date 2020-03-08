using System.IO;

namespace Prelude.Gameplay.Charts.YAVSRG
{
    public class Snap : OffsetItem
    {
        public BinarySwitcher taps;
        public BinarySwitcher holds;
        public BinarySwitcher middles;
        public BinarySwitcher ends;
        public BinarySwitcher mines;
        public BinarySwitcher special; //16 bits for special gives room for scratches + SDVX wavy lane info if i ever implement it
        //scratch will be implemented if i add bms support - this is otherwise just future proofing
        public ushort hitsound = 0;

        public Snap(float offset, ushort taps = 0, ushort holds = 0, ushort middles = 0, ushort ends = 0, ushort mines = 0, ushort special = 0, ushort hitsound = 0) : base(offset)
        {
            this.taps = new BinarySwitcher(taps);
            this.holds = new BinarySwitcher(holds);
            this.middles = new BinarySwitcher(middles);
            this.ends = new BinarySwitcher(ends);
            this.mines = new BinarySwitcher(mines);
            this.special = new BinarySwitcher(special);
            this.hitsound = hitsound;
        }
        
        public override OffsetItem Interpolate(float time)
        {
            return new Snap(time, 0, 0, (ushort)(holds.value + middles.value), 0, 0, 0, 0);
        }

        public Snap Mask(ushort mask)
        {
            return new Snap(Offset, (ushort)(taps.value & mask), (ushort)(holds.value & mask), (ushort)(middles.value & mask), (ushort)(ends.value & mask), (ushort)(mines.value & mask), special.value, hitsound);
        }

        public bool IsEmpty()
        {
            return (taps.value | holds.value | ends.value | mines.value | special.value) == 0;
        }

        public int Count
        {
            get
            {
                return taps.Count + holds.Count + ends.Count; //separate to combine in case they become different
            }
        }

        public BinarySwitcher Combine()
        {
            return new BinarySwitcher(taps.value | holds.value | ends.value);
        }

        public BinarySwitcher this[int index]
        {
            get
            {
                switch (index)
                {
                    case 0:
                    default:
                        return taps;
                    case 1: return holds;
                    case 2: return middles;
                    case 3: return ends;
                    case 4: return mines;
                    case 5: return special;
                }
            }
            set
            {
                if (index > 5) return; //future proof for other things i could store (hitsounds)
                SetValue(index, value.value);
            }
        }

        public void SetValue(int index, ushort value)
        {
            this[index].value = value;
        }

        public Snap ReadFromFile(BinaryReader file)
        {
            byte data = file.ReadByte();
            for (byte i = 0; i < 6; i++)
            {
                if (((1 << i) & data) > 0)
                {
                    SetValue(i, file.ReadUInt16());
                }
            }
            return this;
        }

        public void WriteToFile(BinaryWriter file)
        {
            byte data = 0;
            for (byte i = 0; i < 6; i++)
            {
                if (this[i].value > 0)
                {
                    data |= (byte)(1 << i);
                }
            }
            file.Write(data);
            for (byte i = 0; i < 6; i++)
            {
                if (this[i].value > 0)
                {
                    file.Write(this[i].value);
                }
            }
        }
    }
}
