using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Net.Web;

namespace Interlude.Interface.Widgets
{
    class DownloadManager : Widget
    {
        string searchtext = "";
        FlowContainer sc;

        public DownloadManager(EtternaPackData data)
        {
            sc = new FlowContainer();
            Reposition(0, 1, 0, 0, 620, 1, 0, 1);
            foreach (EtternaPackData.EtternaPack p in data.data)
            {
                sc.AddChild(new DownloadCard(p).Reposition(0, 0, 0, 0, 0, 1, 50, 0));
            }
            AddChild(new TextEntryBox((s) => { searchtext = s; }, () => { return searchtext; },
                Filter, null, () => { return "Press " + Game.Options.General.Hotkeys.Search.ToString().ToUpper() + " to search..."; })
                .Reposition(0, 0, 0, 0, 0, 1, 60, 0));
            AddChild(sc.Reposition(0, 0, 60, 0, 0, 1, 0, 1));
        }

        void Filter()
        {
            string f = searchtext.ToLower();
            sc.Filter((w) => ((DownloadCard)w).name.ToLower().Contains(f));
        }
    }
}
