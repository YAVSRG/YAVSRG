using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap
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
            value |= (1 << i);
        }

        public void RemoveColumn(int i)
        {
            if (GetColumn(i))
            {
                value -= (1 << i);
            }
        }

        public void ToggleColumn(int i)
        {
            value ^= (1 << i);
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
}
