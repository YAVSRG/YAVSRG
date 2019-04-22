using System;
using Prelude.Utilities;
using Interlude.Interface.Widgets;

namespace Interlude.Interface.Dialogs
{
    public class ConfigDialog : FadeDialog
    {
        public ConfigDialog(Action<string> action, string Name, DataGroup Data, DataTemplateAttribute[] Attributes) : base(action)
        {
            PositionTopLeft(300, 100, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(300, 100, AnchorType.MAX, AnchorType.MAX);
            AddChild(new DataGroupConfig(Data, Attributes));
            AddChild(new TextBox(Name, AnchorType.CENTER, 30, true, Game.Options.Theme.MenuFont)
                .PositionTopLeft(0, -60, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MIN));
        }

        public ConfigDialog(Action<string> action, string Name, DataGroup Data, Type Type) : this(action, Name, Data, DataTemplateAttribute.GetAttributes(Type)) { }
    }
}
