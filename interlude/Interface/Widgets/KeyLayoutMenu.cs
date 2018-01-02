using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class KeyLayoutMenu : WidgetContainer
    {
        int keymode;

        public KeyLayoutMenu() : base()
        {
            ChangeKeyMode(4);
        }

        public void ChangeKeyMode(int k)
        {
            keymode = k;
            for (int i = 0; i < k; i++)
            {
                var w = new KeyBinder("Column " + (i + 1).ToString(), Game.Options.Profile.Bindings[k][i], SomeFunnyClosureThing(i,k));
                Widgets.Add(w.PositionTopLeft(50 + 200 * i, 50, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(150 + 200 * i, 150, AnchorType.MIN, AnchorType.MIN));
            }
        }

        private Action<OpenTK.Input.Key> SomeFunnyClosureThing(int i, int k)
        {
            return (key) => { Game.Options.Profile.Bindings[k][i] = key; };
        }
    }
}
