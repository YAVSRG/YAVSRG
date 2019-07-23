using System.Collections.Generic;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Utilities;

namespace Prelude.Gameplay.Mods
{
    //This modifier is incomplete and undocumented
    public class Keychange : Mod
    {
        ushort[] jack = new ushort[] { 1, 2, 8, 16 };
        ushort[] notes = new ushort[] { 3, 6, 12, 24 };
        ushort[] backupNotes = new ushort[] { 7, 14, 14, 28 };
        ushort[] criteria = new ushort[] { 1, 2, 4, 8 };

        public override void Apply(ChartWithModifiers Chart, DataGroup Data)
        {
            foreach (GameplaySnap s in Chart.Notes.Points)
            {
                for (int i = 0; i < 6; i++)
                {
                    var x = s[i];
                    if (x.GetColumn(0)) x.SetColumn(6);
                    if (x.GetColumn(1)) x.SetColumn(5);
                    if (x.GetColumn(2)) x.SetColumn(4);
                }
                Chart.Keys = 7;
                Chart.Timing.SV = new SVManager(7).SV;
            }
            /*
            ushort prev = 0;
            List<GameplaySnap> newNotes = new List<GameplaySnap>();
            Chart.Keys = 5;
            foreach (GameplaySnap s in Chart.Notes.Points)
            {
                prev = GetValue((ushort)(s.taps.value | s.holds.value), prev);
                newNotes.Add(new GameplaySnap(s.Offset, prev, 0, 0, 0, 0));
            }
            Chart.Notes.Points = newNotes;*/
        }

        /*
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
        }*/

        public override int GetStatus(DataGroup Data)
        {
            return 2;
        }

        public override string GetName(DataGroup Data)
        {
            return "KeyChange";
        }
    }
}
