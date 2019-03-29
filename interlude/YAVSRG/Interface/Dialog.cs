using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Interface.Widgets;

namespace Interlude.Interface
{
    public class Dialog : Widget
    {
        public Action<string> OnComplete;
        public bool Closed = false;

        public Dialog(Action<string> action) : base()
        {
            OnComplete = action;
        }

        protected void Close(string s)
        {
            Closed = true;
            OnComplete(s);
        }
    }
}
