using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface;

namespace YAVSRG.Options
{
    public class WidgetPosition
    {
        public int Top = 0; public AnchorType TopAnchor = AnchorType.MIN;
        public int Left = 0; public AnchorType LeftAnchor = AnchorType.MIN;
        public int Right = 0; public AnchorType RightAnchor = AnchorType.MAX;
        public int Bottom = 0; public AnchorType BottomAnchor = AnchorType.MAX;
        public bool Enable;
    }
}
