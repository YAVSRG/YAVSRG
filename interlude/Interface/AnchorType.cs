using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface
{
    public enum AnchorType
    {
        MIN, //top left
        CENTER,
        MAX, //bottom right
        LERP
    }

    public enum WidgetState
    {
        DISABLED,
        NORMAL,
        ACTIVE
    }
}
