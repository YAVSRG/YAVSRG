using System.Collections.Generic;

namespace Prelude.Gameplay.Charts.YAVSRG
{
    public class BinarySwitcher
    {
        public ushort value;

        public BinarySwitcher(ushort v)
        {
            value = v;
        }

        public BinarySwitcher(int v)
        {
            value = (ushort)v;
        }

        public bool GetColumn(byte i)
        {
            return (value & (1 << i)) > 0;
        }

        public void SetColumn(byte i)
        {
            value |= (ushort)(1 << i);
        }

        public void RemoveColumn(byte i)
        {
            if (GetColumn(i))
            {
                value -= (ushort)(1 << i);
            }
        }

        public void ToggleColumn(byte i)
        {
            value ^= (ushort)(1 << i);
        }

        public IEnumerable<byte> GetColumns()
        {
            int temp = value;
            byte i = 0;
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
}
