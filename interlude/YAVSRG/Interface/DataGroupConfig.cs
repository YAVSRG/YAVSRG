using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prelude.Utilities;
using Interlude.Interface.Widgets;

namespace Interlude.Interface
{
    public class DataGroupConfig : FlowContainer
    {
        class DeletableSetting : Widget
        {
            public DeletableSetting(Widget w, string name, DataGroup data)
            {
                AddChild(w.Reposition(0, 0, 0, 0, -50, 1, 0, 1));
                AddChild(new SpriteButton("buttonclose", () => { SetState(WidgetState.DISABLED); data.Remove(name); }, null) { Tooltip = "Delete " + name }
                    .Reposition(-50, 1, 0, 0, 0, 1, 0, 1));
                Reposition(0, 0, 0, 0, 0, 1, 50, 0);
            }
        }

        public DataGroupConfig(DataGroup Data, DataTemplateAttribute[] Template)
        {
            foreach (var t in Template)
            {
                Widget w;
                var type = t.Properties.GetValue("Default", default(object)).GetType();
                if (type == typeof(float))
                {
                    w = new Slider(t.Name,
                        (i) =>
                        {
                            Data.Remove(t.Name);
                            Data[t.Name] = i;
                        },
                        () => Data.GetValue(t.Name, GetDefault<float>(t.Properties)),
                        t.Properties.GetValue("Min", 0f),
                        t.Properties.GetValue("Max", 1f),
                        t.Properties.GetValue("Step", 0.1f));
                }
                else if (type == typeof(int))
                {
                    w = new Slider(t.Name,
                        (i) => {
                            Data.Remove(t.Name);
                            Data.Add(t.Name, (int)i);
                        },
                        () => Data.GetValue(t.Name, GetDefault<int>(t.Properties)),
                        t.Properties.GetValue("Min", 0),
                        t.Properties.GetValue("Max", 100),
                        1);
                }
                else
                {
                    w = new TextBox(t.Name, AnchorType.CENTER, 30, true, System.Drawing.Color.White);
                }
                AddChild(new DeletableSetting(w, t.Name, Data));
            }
        }

        private T GetDefault<T>(DataGroup d)
        {
            try
            {
                return d.GetValue("Default", default(T));
            }
            catch
            {
                return default(T);
            }
        }
    }
}
