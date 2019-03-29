using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Prelude.Gameplay.Mods
{
    public class Keychange : Mod
    {
        ushort[] jack = new ushort[] { 1, 2, 8, 16 };
        ushort[] notes = new ushort[] { 3, 6, 12, 24 };
        ushort[] backupNotes = new ushort[] { 7, 14, 14, 28 };
        ushort[] criteria = new ushort[] { 1, 2, 4, 8 };

        public override bool IsApplicable(ChartWithModifiers c, string data)
        {
            return base.IsApplicable(c, data);
        }

        public override void Apply(ChartWithModifiers c, string data)
        {
            base.Apply(c, data);
            ushort prev = 0;
            List<GameplaySnap> newNotes = new List<GameplaySnap>();
            c.Keys = 5;
            foreach (GameplaySnap s in c.Notes.Points)
            {
                prev = GetValue((ushort)(s.taps.value | s.holds.value), prev);
                newNotes.Add(new GameplaySnap(s.Offset, prev, 0, 0, 0, 0));
            }
            c.Notes.Points = newNotes;
        }

        ushort GetValue(ushort input, ushort previous)
        {
            ushort output = 0;
            for (int i = 0; i < criteria.Length; i++)
            {
                if ((input & criteria[i]) > 0)
                {
                    output |= notes[i];
                }
            }
            output &= (ushort)~previous;
            for (int i = 0; i < criteria.Length; i++)
            {
                if ((input & criteria[i]) > 0)
                {
                    if ((previous & backupNotes[i]) == backupNotes[0])
                    {
                        output |= jack[i];
                    }
                    else
                    {
                        output |= (ushort)(backupNotes[i] & ~previous);
                    }
                }
            }
            return output;
        }

        public override string GetName(string data)
        {
            return "KeyChange";
        }

        public override string GetDescription(string data)
        {
            return base.GetDescription(data);
        }
    }
}
