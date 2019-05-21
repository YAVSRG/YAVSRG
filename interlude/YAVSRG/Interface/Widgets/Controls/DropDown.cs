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
        private string label;
        private FlowContainer selector;

        public DropDown(Action<string> set, Func<string> get, string label)
        {
            this.label = label;
            setter = set;
            getter = get;
            selector = new FlowContainer() { MarginY = 25, MarginX = 20, VerticalFade = 30, Frame = 85 };
            selector.ToggleState();
            AddChild(selector.TL_DeprecateMe(0, 0, AnchorType.MIN, AnchorType.MAX).BR_DeprecateMe(0, -400, AnchorType.MAX, AnchorType.MAX));
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
            return new SimpleButton(label, () => { setter(label); }, () => { return getter() == label; }, 15f).BR_DeprecateMe(0, 35, AnchorType.MAX, AnchorType.MIN);
        }
    }
}
