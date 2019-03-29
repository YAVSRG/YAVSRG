using System.Linq;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Mods
{
    public class Random : Mod
    {
        System.Random rand;

        public override void Apply(ChartWithModifiers c, string data)
        {
            base.Apply(c, data);
            rand = new System.Random(0);
            ushort mask = (ushort)((1 << c.Keys) - 1);
            ushort lastRow = 0;
            ushort lastOriginalRow = 0;
            ushort newRow;
            ushort allowedBits;

            foreach (GameplaySnap s in c.Notes.Points)
            {
                newRow = 0;

                //jacks
                allowedBits = (ushort)(mask & lastRow & ~(s.middles.value | s.ends.value | s.holds.value | s.mines.value));
                for (int i = new BinarySwitcher(s.taps.value & lastOriginalRow).Count; i > 0; i--)
                {
                    ushort r = randomBit(allowedBits);
                    allowedBits &= (ushort)~r;
                    newRow |= r;
                }

                //not jacks
                allowedBits = (ushort)((mask ^ lastRow) & ~(s.middles.value | s.ends.value | s.holds.value | s.mines.value));
                for (int i = new BinarySwitcher(s.taps.value & ~lastOriginalRow).Count; i > 0; i--)
                {
                    ushort r = randomBit(allowedBits);
                    allowedBits &= (ushort)~r;
                    newRow |= r;
                }

                lastRow = newRow;
                lastOriginalRow = s.taps.value;
                s.taps.value = newRow;
            }
        }

        ushort randomBit(ushort row)
        {
            var l = new BinarySwitcher(row).GetColumns().ToList();
            if (l.Count == 0) return 0;
            return (ushort)(1 << l[rand.Next(0,l.Count)]);
        }

        public override int GetStatus(string data)
        {
            return 2;
        }

        public override string GetName(string data)
        {
            return "Randomiser";
        }
    }
}
