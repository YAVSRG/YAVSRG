using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Options
{
    public class WidgetPositionData
    {
        public Dictionary<string, WidgetPosition> data = new Dictionary<string, WidgetPosition>();

        public bool IsEnabled(string name)
        {
            if (data.ContainsKey(name))
            {
                return data[name].Enable;
            }
            return false;
        }
    }
}
