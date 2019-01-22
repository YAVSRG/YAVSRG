using System.Collections.Generic;
using YAVSRG.Gameplay.Charts.YAVSRG;

namespace YAVSRG.Gameplay
{
    public class ChartSaveData
    {
        public string Path;
        public float Offset;
        public List<Score> Scores = new List<Score>();
        //todo: grade achieved and stuff in here

        public static ChartSaveData FromChart(Chart c)
        {
            return new ChartSaveData()
            {
                Path = c.GetFileIdentifier(), //this needs to be the absolute path to the file (just self reminder)
                Offset = c.Notes.Count > 0 ? c.Notes.Points[0].Offset : 0
            };
        }
    }
}
