using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Beatmap
{
    public class MultiChart
    {
        public List<Chart> diffs;
        public ChartHeader header;

        public MultiChart(ChartHeader header)
        {
            this.header = header;
            diffs = new List<Chart>();
        }
    }
}
