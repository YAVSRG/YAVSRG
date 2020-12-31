using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Widgets
{
    public class DropDown : Widget
    {
        private Action<string> setter;
        private Func<string> getter;
        private FlowContainer selector;

        public DropDown(Action<string> set, Func<string> get, string label)
        {
            setter = set;
            getter = get;
            selector = new FlowContainer() { MarginY = 25, MarginX = 20, VerticalFade = 30, Frame = 85 };
            selector.ToggleState();
            AddChild(selector.Reposition(0, 0, 0, 1, 0, 1, 400, 1));
            AddChild(new SimpleButton(() => label + ": " + getter(), () => { selector.ToggleState(); }, () => selector.State == WidgetState.NORMAL, null));
        }

        public DropDown SetItems(List<string> items)
        {
            selector.Clear();
            foreach (string item in items)
            {
                selector.AddChild(Item(item));
            }
            return this;
        }

        private Widget Item(string label)
        {
            return new SimpleButton(label, () => { setter(label); }, () => { return getter() == label; }, null) { FontSize = 15 }.Reposition(0, 0, 0, 0, 0, 1, 35, 0);
        }
    }
}
