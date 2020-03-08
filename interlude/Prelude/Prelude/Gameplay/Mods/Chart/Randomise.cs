using System;
using System.Linq;
using Prelude.Utilities;
using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay.Mods
{
    //Rearranges notes to preserve direct jacks and direct streams
    //Does not attempt to preserve note density per column but in general this is a good randomiser for non-speed patterns
    public class Randomise : Mod
    {
        Random rand;

        public override void Apply(ChartWithModifiers Chart, DataGroup Data)
        {
            rand = new Random(Data.GetValue("Seed", 0));
            ushort mask = (ushort)((1 << Chart.Keys) - 1);
            ushort lastRow = 0;
            ushort lastOriginalRow = 0;
            ushort newRow;
            ushort allowedBits;

            foreach (GameplaySnap s in Chart.Notes.Points)
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

        public override int Status => 2;

        public override bool Visible => true;

        public override string Name => "Randomise";

        public override DataGroup DefaultSettings => new DataGroup() { { "Seed", new Random().Next() } };
    }
}
