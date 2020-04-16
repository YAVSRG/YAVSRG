using System.Linq;
using System.Collections.Generic;
using Prelude.Gameplay.Charts.YAVSRG;
using System.IO;

namespace Interlude.Editor
{
    public class ChartInEditor
    {
        public EditorData EditorData;
        public List<PointManager<Snap>> Layers;
        public SVManager Timing;
        public int Keys;

        public ChartInEditor(Chart from)
        {
            EditorData = EditorData.FromChart(from.Data);
            Timing = from.Timing; //unsafe
            Keys = from.Keys;
            Layers.Add(from.Notes);
        }
    }
}
