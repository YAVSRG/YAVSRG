using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay
{
    public class HitData : OffsetItem
    {
        public float[] delta;
        public byte[] hit;

        public HitData(GameplaySnap s, int keycount) : base(s.Offset)
        {
            hit = new byte[keycount];
            foreach (int k in s.Combine().GetColumns())
            {
                hit[k] = 1;
            }
            delta = new float[keycount];
        }

        public int Count
        {
            get
            {
                int x = 0;
                for (int i = 0; i < hit.Length; i++)
                {
                    if (hit[i] > 0)
                    {
                        x++;
                    }
                }
                return x;
            }
        }
    }
}
