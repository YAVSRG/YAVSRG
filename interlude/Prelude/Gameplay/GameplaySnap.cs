using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay
{
    //this extends the snap data structure but now it records color scheme information e.g. column coloring and DDR coloring
    //note colors are precalculated rather than done live
    //gameplay snaps are created as the final "to be played" chart when a loaded chart goes through modifier processing
    //user selects chart -> user changes mods -> GameplayChart is updated in memory to reflect mods -> this is what user plays
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
