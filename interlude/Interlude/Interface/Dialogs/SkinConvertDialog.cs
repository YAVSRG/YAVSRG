using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Dialogs
{
    public class SkinConvertDialog : Dialog
    {
        string name;

        public SkinConvertDialog(Action<string> action) : base(action)
        {
            Game.Screens.AddDialog(new TextDialog("Enter skin name: ", (s) => { name = s; }));
        }

        public override void Update(Rect bounds)
        {
            if (name == "")
            {
                Close("");
            }
            base.Update(bounds);
        }
    }
}
