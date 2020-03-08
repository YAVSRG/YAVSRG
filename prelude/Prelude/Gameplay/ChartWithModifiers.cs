using Prelude.Gameplay.Charts.YAVSRG;

namespace Prelude.Gameplay
{
    //this is a reduced chart structure used when a chart (containing metadata and stuff) has been processed by gameplay modifiers
    //this holds the resulting notes and SVs and also tracks what mods have been applied and the new (after mods, most likely the same) key count.
    public class ChartWithModifiers
    {
        public int Keys;
        public PointManager<GameplaySnap> Notes;
        public SVManager Timing;
        private string AppliedMods;
        public int ModStatus;

        public ChartWithModifiers(Chart baseChart)
        {
            AppliedMods = "";
            Keys = baseChart.Keys;
            Timing = new SVManager(baseChart.Timing);
            //todo: duplicate timing points too since otherwise mods can edit original chart in memory
            //currently mods must watch out for referencing errors
            Notes = new PointManager<GameplaySnap>();
            foreach (Snap s in baseChart.Notes.Points)
            {
                Notes.Points.Add(new GameplaySnap(s)); //todo: maybe identity optimisation
            }
            Notes.Count = baseChart.Notes.Count;
        }

        //assign is used to append mods to the applied list
        //get is used to give a string representation of mods applied
        public string Mods
        {
            get { return AppliedMods; }
            set { AppliedMods += ", " + value; }
        }
    }
}
