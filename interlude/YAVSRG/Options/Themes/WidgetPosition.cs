using System;
using System.Collections.Generic;
using Prelude.Utilities;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Interface;

namespace Interlude.Options
{
    public class WidgetPosition
    {
        public float Top = 0; public float TopRel = 0;
        public float Left = 0; public float LeftRel = 0;
        public float Right = 0; public float RightRel = 1;
        public float Bottom = 0; public float BottomRel = 1;
        public bool Enable = false;
        public DataGroup Extra = new DataGroup();
    }
}
