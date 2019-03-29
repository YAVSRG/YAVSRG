using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay
{
    public class GameplaySnap : Snap
    {
        public int[] colors;
        public GameplaySnap(float offset, ushort taps, ushort holds, ushort middles, ushort ends, ushort mines): base(offset, taps, holds, middles, ends, mines)
        {
            colors = new int[10];
        }

        public GameplaySnap(Snap s) : base(s.Offset, s.taps.value, s.holds.value, s.middles.value, s.ends.value, s.mines.value)
        {
            colors = new int[10];
        }

        public override OffsetItem Interpolate(float time)
        {
            return new GameplaySnap(time, 0, 0, (ushort)(holds.value + middles.value), 0, 0);
        }
    }
}
