using System.Collections.Generic;

namespace Interlude.Options
{
    public class WidgetPositionData
    {
        public Dictionary<string, WidgetPosition> Data = new Dictionary<string, WidgetPosition>();

        //todo: do this using two Rects instead of 8 parameters
        public WidgetPosition GetWidgetConfig(string name, float l, float la, float t, float ta, float r, float ra, float b, float ba, bool enable)
        {
            if (!Data.ContainsKey(name))
            {
                Data[name] = new WidgetPosition() { Left = l, LeftRel = la, Top = t, TopRel = ta, Right = r, RightRel = ra, Bottom = b, BottomRel = ba, Enable = enable };
            }
            return Data[name];
        }
    }
}
