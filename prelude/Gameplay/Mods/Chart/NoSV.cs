using Prelude.Utilities;

namespace Prelude.Gameplay.Mods
{
    public class NoSV : Mod
    {
        //Removes SV changes from a chart
        public override bool IsApplicable(ChartWithModifiers Chart, DataGroup Data)
        {
            return Chart.Timing.ContainsSV();
        }

        public override void Apply(ChartWithModifiers Chart, DataGroup Data)
        {
            Chart.Timing.SetBlankSVData();
        }

        public override string GetName(DataGroup Data)
        {
            return "NoSV";
        }

        public override string GetDescription(DataGroup Data) { return "Removes all scroll speed changes from a chart."; }
    }
}
