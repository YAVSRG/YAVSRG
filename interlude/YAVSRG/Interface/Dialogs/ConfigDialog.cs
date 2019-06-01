using System;
using Prelude.Utilities;
using Interlude.Interface.Widgets;

namespace Interlude.Interface.Dialogs
{
    public class ConfigDialog : FadeDialog
    {
        public ConfigDialog(Action<string> action, string Name, DataGroup Data, DataTemplateAttribute[] Attributes) : base(action)
        {
            if (Attributes.Length == 0) { Close(""); }
            Reposition(300, 0, 100, 0, -300, 1, -100, 1);
            AddChild(new DataGroupConfig(Data, Attributes));
            AddChild(new TextBox(Name, AnchorType.CENTER, 30, true, Game.Options.Theme.MenuFont)
                .Reposition(0, 0, -60, 0, 0, 1, 0, 0));
        }

        public ConfigDialog(Action<string> action, string Name, DataGroup Data, Type Type) : this(action, Name, Data, DataTemplateAttribute.GetAttributes(Type)) { }
    }
}
