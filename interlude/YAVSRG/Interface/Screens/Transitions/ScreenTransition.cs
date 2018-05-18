using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Screens.Transitions
{
    class ScreenTransition //nyi :(
    {
        public virtual void Draw(Screen current, Screen previous, Action<float> bgdraw) { }
        public virtual void Update(Screen current) { }
        
    }
}
