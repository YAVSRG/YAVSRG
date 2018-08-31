using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class BoolPicker : TextPicker
    {
        public BoolPicker(string label, bool start, Action<bool> set) : base(label,new string[] { "OFF", "ON" }, start ? 1 : 0, v => set(v == 1))
        {

        }
    }
}
