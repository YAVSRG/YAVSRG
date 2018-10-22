using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class DropDown : Widget
    {
        private Action<string> setter;
        private Func<string> getter;
        private string label;
        private ScrollContainer selector;

        public DropDown(Action<string> set, Func<string> get, string label)
        {
            this.label = label;
            setter = set;
            getter = get;
            selector = new ScrollContainer(20, 10, false);
            selector.ToggleState();
            AddChild(selector.PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(0, -400, AnchorType.MAX, AnchorType.MAX));
            AddChild(new SimpleButton(() => label + ": " + getter(), () => { selector.ToggleState(); }, () => selector.State == WidgetState.NORMAL, 20f));
        }

        public DropDown SetItems(List<string> items)
        {
            foreach (string item in items)
            {
                selector.AddChild(Item(item));
            }
            return this;
        }

        private Widget Item(string label)
        {
            return new SimpleButton(label, () => { setter(label); }, () => { return getter() == label; }, 15f).PositionBottomRight(40, 35, AnchorType.MAX, AnchorType.MIN);
        }
    }
}
