using Prelude.Utilities;

namespace Prelude.Gameplay.Mods
{
    [DataTemplate("all", "Default", 0, "Min", 0, "Max", 1)]
    public class NoLN : Mod
    {
        public override void Apply(ChartWithModifiers Chart, DataGroup Data)
        {
            if (Data.GetValue("all", 0) == 1)
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
            if (Data.GetValue("all", 0) == 0)
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

        public override string GetName(DataGroup Data)
        {
            return Data.GetValue("all", 0) == 0 ? "No Releases" : "No Holds";
        }

        public override int GetStatus(DataGroup Data) => 1;

        public override string GetDescription(DataGroup Data) { return Data.GetValue("all", 0) == 0 ? "Disables the need to accurately time releasing the ends of hold notes" : "Replaces all hold notes with a tap note"; }
    }
}
