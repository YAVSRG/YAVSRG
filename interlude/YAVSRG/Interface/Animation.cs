using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface
{
    public class Animation
    {
        public virtual bool DisposeMe { get { return false; } }
        public virtual bool Running { get { return true; } }
        public virtual void Update() { }
    }
}
