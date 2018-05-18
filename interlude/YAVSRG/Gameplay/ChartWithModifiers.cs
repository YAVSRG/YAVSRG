using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Gameplay
{
    public class ChartWithModifiers
    {
        public int Keys;
        public PointManager<GameplaySnap> Notes;
        public PointManager<BPMPoint> Timing;

        public ChartWithModifiers(Chart baseChart)
        {
            Keys = baseChart.Keys;
            Timing = new PointManager<BPMPoint>(baseChart.Timing.Points); //WATCH OUT FOR REFERENCING ERRORS
            //NO MOD SHOULD EDIT EXISTING TIMING POINTS RATHER THAN RECREATING THEM
            Notes = new PointManager<GameplaySnap>();
            foreach (Snap s in baseChart.Notes.Points)
            {
                Notes.Points.Add(new GameplaySnap(s)); //please don't be too intensive
            }
            Notes.Count = baseChart.Notes.Count;
        }
    }
}
