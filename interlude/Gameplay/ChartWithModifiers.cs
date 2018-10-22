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
        public SVManager Timing;
        private string mods;

        public ChartWithModifiers(Chart baseChart)
        {
            mods = "";
            Keys = baseChart.Keys;
            Timing = new SVManager(baseChart.Timing);
            //WATCH OUT FOR REFERENCING ERRORS
            //NO MOD SHOULD EDIT EXISTING TIMING POINTS RATHER THAN RECREATING THEM
            Notes = new PointManager<GameplaySnap>();
            foreach (Snap s in baseChart.Notes.Points)
            {
                Notes.Points.Add(new GameplaySnap(s)); //please don't be too intensive
            }
            Notes.Count = baseChart.Notes.Count;
        }

        public string Mods
        {
            get { return mods; }
            set { mods += ", " + value; }
        }
    }
}
