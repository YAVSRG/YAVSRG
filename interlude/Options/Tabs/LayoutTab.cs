using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Input;
using YAVSRG.Interface;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Options.Tabs
{
    class LayoutTab : WidgetContainer
    {
        private Widget selectKeyMode;
        private ColumnOptions[] columns = new ColumnOptions[10];
        private int keyMode;

        private class ColumnOptions : WidgetContainer
        {
            private KeyBinder bind;

            public ColumnOptions(int k)
            {
                bind = new KeyBinder("Column " + (k + 1).ToString(), Key.F35, (b) => { });
                Widgets.Add(bind.PositionTopLeft(0, 50, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
            }

            public void Update(Key b, Action<Key> onBind)
            {
                bind.Change(b, onBind);
            }
        }

        public LayoutTab() : base()
        {
            selectKeyMode = new TextPicker("Keys", new string[] { "3K", "4K", "5K", "6K", "7K", "8K", "9K", "10K" }, 1, (i) => { ChangeKeyMode(i + 3); })
                .PositionTopLeft(-50,25,AnchorType.CENTER,AnchorType.MIN).PositionBottomRight(50,75,AnchorType.CENTER,AnchorType.MIN);
            for (int i = 0; i < 10; i++)
            {
                columns[i] = new ColumnOptions(i);
                Widgets.Add(columns[i]);
            }
            Widgets.Add(selectKeyMode);
            ChangeKeyMode(4); //profile based stuff nyi
        }
        
        private Action<Key> SomeFunnyClosureThing(int i, int k)
        {
            return (key) => { Game.Options.Profile.Bindings[k][i] = key; };
        }

        private void ChangeKeyMode(int k)
        {
            for (int i = 0; i < 10; i++)
            {
                columns[i].State = 0;
            }
            keyMode = k;
            int c = Game.Options.Theme.ColumnWidth;
            int start = -k * c / 2;
            for (int i = 0; i < k; i++)
            {
                columns[i].Update(Game.Options.Profile.Bindings[k][i], SomeFunnyClosureThing(i, k));
                columns[i].State = 1;
                columns[i].PositionTopLeft(start + i * c, 100, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(start + c + i * c, 100, AnchorType.CENTER, AnchorType.MAX);
            }
        }
    }
}
