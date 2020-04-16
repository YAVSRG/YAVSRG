using System;
using Prelude.Utilities;
using Interlude.Options;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    class HotkeysPanel : Widget
    {
        public HotkeysPanel()
        {
            Widget f;
            AddChild(f = new FlowContainer() { MarginY = 50, RowSpacing = 50 }.Reposition(50, 0, 50, 0, -50, 1, -50, 1));
            foreach (var a in typeof(Keybinds).GetFields())
            {
                f.AddChild(new KeyBinder(a.Name, new SetterGetter<Bind>(Game.Options.General.Hotkeys, a.Name)).Reposition(0, 0, 0, 0, 280, 0, 50, 0));
            }
        }
    }
}
