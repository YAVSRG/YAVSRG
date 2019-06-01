using Prelude.Utilities;

namespace Prelude.Gameplay.Mods
{
    //Mirrors the entire chart horizontally
    public class Mirror : Mod
    {
        public override void Apply(ChartWithModifiers Chart, DataGroup Data)
        {
            foreach (GameplaySnap s in Chart.Notes.Points)
            {
                s.taps.value = BitMirror(s.taps.value, Chart.Keys);
                s.ends.value = BitMirror(s.ends.value, Chart.Keys);
                s.holds.value = BitMirror(s.holds.value, Chart.Keys);
                s.mines.value = BitMirror(s.mines.value, Chart.Keys);
                s.middles.value = BitMirror(s.middles.value, Chart.Keys);
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

        public override string GetName(DataGroup Data)
        {
            return "Mirror";
        }

        public override string GetDescription(DataGroup Data) { return "Horizontally flips the whole chart"; }
    }
}
