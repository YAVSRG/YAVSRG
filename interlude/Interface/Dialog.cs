using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface
{
    public class Dialog : WidgetContainer
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
