using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Options
{
    public class WidgetPositionData
    {
        public Dictionary<string, WidgetPosition> Data = new Dictionary<string, WidgetPosition>();

        public bool IsEnabled(string name)
        {
            if (Data.ContainsKey(name))
            {
                return Data[name].Enable;
            }
            return false;
        }

        public WidgetPosition GetPosition(string name)
        {
            if (Data.ContainsKey(name))
            {
                return Data[name];
            }
            return new WidgetPosition();
        }
    }
}
