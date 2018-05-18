using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Gameplay.Mods
{
    public class Manipulate : Mod
    {
        public override void Apply(ChartWithModifiers c)
        {
            List<GameplaySnap> newPoints = new List<GameplaySnap>();
            int count = c.Notes.Count;
            for (int i = 1; i < count; i++)
            {
                GameplaySnap a = c.Notes.Points[i];
                GameplaySnap b = c.Notes.Points[i - 1];
                if (a.Offset - b.Offset < 100)
                {
                    if (b.middles.value == 0 && b.Mask(Val(a)).IsEmpty())
                    {
                        newPoints.Add(new GameplaySnap((a.Offset + b.Offset) * 0.5f,(ushort)(a.taps.value + b.taps.value), (ushort)(a.holds.value + b.holds.value), (ushort)(a.middles.value + b.middles.value), (ushort)(a.ends.value + b.ends.value), (ushort)(a.mines.value + b.mines.value)));
                        i++; continue;
                    }
                }
                newPoints.Add(b);
            }//missing last snap but who cares
            c.Notes = new PointManager<GameplaySnap>(newPoints);
        }

        private ushort Val(GameplaySnap s)
        {
            return (ushort)(s.ends.value + s.holds.value + s.taps.value + s.mines.value + s.middles.value);
        }

        public override string GetName()
        {
            return "Manipulate";
        }
    }
}
