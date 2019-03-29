using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Mods
{
    public class Mirror : Mod
    {
        public override void Apply(ChartWithModifiers c, string data)
        {
            base.Apply(c, data);
            foreach (GameplaySnap s in c.Notes.Points)
            {
                s.taps.value = BitMirror(s.taps.value, c.Keys);
                s.ends.value = BitMirror(s.ends.value, c.Keys);
                s.holds.value = BitMirror(s.holds.value, c.Keys);
                s.mines.value = BitMirror(s.mines.value, c.Keys);
                s.middles.value = BitMirror(s.middles.value, c.Keys);
            }
        }

        private ushort BitMirror(ushort v, int k)
        {
            ushort o = 0;
            for (int i = 0; i < k; i++)
            {
                if ((1 << i & v) > 0)
                {
                    o += (ushort)(1 << (k - 1 - i));
                }
            }
            return o;
        }

        public override string GetName(string data)
        {
            return "Mirror";
        }

        public override string GetDescription(string data) { return "Horizontally flips an entire chart."; }
    }
}
