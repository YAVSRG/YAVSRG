using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Input;

namespace Interlude.Options
{
    public class Keybinds
    {
        //incomplete bind list
        public Key Exit = Key.Escape;
        public Key Select = Key.Enter;
        public Key UpRate = Key.Plus;
        public Key DownRate = Key.Minus;
        public Key ChangeOffset = Key.Plus;
        public Key Volume = Key.AltLeft;
        public Key Skip = Key.Space;
        public Key Search = Key.Tab;
        public Key CollapseToToolbar = Key.Insert;
        public Key Screenshot = Key.F12;
        public Key Chat = Key.F8;
    }
}
