using Prelude.Utilities;

namespace Prelude.Gameplay.Mods
{
    [DataTemplate("All", "Default", false)]
    public class NoLN : Mod
    {
        public override void Apply(ChartWithModifiers Chart, DataGroup Data)
        {
            if (Data.GetValue("All", false))
            {
                foreach (GameplaySnap s in Chart.Notes.Points)
                {
                    s.ends.value = 0;
                    s.middles.value = 0;
                    s.taps.value |= s.holds.value;
                    s.holds.value = 0;
                }
            }
        }

        public override void ApplyToHitData(ChartWithModifiers Chart, ref HitData[] HitData, DataGroup Data)
        {
            if (!Data.GetValue("All", false))
            {
                for (int i = 0; i < HitData.Length; i++)
                {
                    for (byte k = 0; k < HitData[i].hit.Length; k++)
                    {
                        if (Chart.Notes.Points[i].ends.GetColumn(k))
                        {
                            HitData[i].hit[k] = 0;
                        }
                    }
                }
            }
        }

        public override bool IsApplicable(ChartWithModifiers Chart, DataGroup Data)
        {
            foreach (GameplaySnap s in Chart.Notes.Points)
            {
                if (s.holds.value > 0)
                {
                    return true;
                }
            }
            return false;
        }

        public override string Name => "No Holds";

        public override int Status => 1;

        public override string Description => "Replaces all hold notes with a tap note";

        public override bool Visible => true;
    }
}
