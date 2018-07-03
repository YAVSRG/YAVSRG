using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Gameplay
{
    public class ChartSaveData
    {
        public string Path;
        public float Offset;
        public List<Score> Scores = new List<Score>();

        public static ChartSaveData FromChart(Chart c)
        {
            return new ChartSaveData()
            {
                Path = System.IO.Path.Combine(c.Data.SourcePath, c.Data.File), //this needs to be the absolute path
                Offset = c.Notes.Count > 0 ? c.Notes.Points[0].Offset : 0
            };
        }
    }
}
