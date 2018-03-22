using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap;

namespace YAVSRG.Gameplay
{
    public class GameplaySnap : Snap
    {
        public int[] colors;
        public GameplaySnap(float offset, int taps, int holds, int middles, int ends): base(offset, taps, holds, middles, ends)
        {
            colors = new int[10];
        }

        public GameplaySnap(Snap s) : base(s.Offset, s.taps.value, s.holds.value, s.middles.value, s.ends.value)
        {
            colors = new int[10];
        }
    }
}
