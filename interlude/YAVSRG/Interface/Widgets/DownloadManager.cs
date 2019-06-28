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
            //sc = new ScrollContainer(10f, 10f, false);
            sc = new FlowContainer();
            TL_DeprecateMe(0, 0, AnchorType.MAX, AnchorType.MIN).BR_DeprecateMe(-620, 0, AnchorType.MAX, AnchorType.MAX);
            foreach (EtternaPackData.EtternaPack p in data.data)
            {
                sc.AddChild(new DownloadCard(p).BR_DeprecateMe(600, 50, AnchorType.MIN, AnchorType.MIN));
            }
            AddChild(new TextEntryBox((s) => { searchtext = s; }, () => { return searchtext; },
                Filter, null, () => { return "Press " + Game.Options.General.Keybinds.Search.ToString().ToUpper() + " to search..."; })
                .TL_DeprecateMe(0,0,AnchorType.MIN,AnchorType.MIN).BR_DeprecateMe(0,60,AnchorType.MAX,AnchorType.MIN));
            AddChild(sc.TL_DeprecateMe(0, 60, AnchorType.MIN, AnchorType.MIN));
        }

        public DownloadManager(List<BloodcatChartData> data)
        {
            //sc = new ScrollContainer(10f, 10f, false);
            sc = new FlowContainer();
            TL_DeprecateMe(0, 0, AnchorType.MAX, AnchorType.MIN).BR_DeprecateMe(-620, 0, AnchorType.MAX, AnchorType.MAX);
            foreach (BloodcatChartData p in data)
            {
                sc.AddChild(new DownloadCard(p).BR_DeprecateMe(600, 50, AnchorType.MIN, AnchorType.MIN));
            }
            AddChild(new TextEntryBox((s) => { searchtext = s; }, () => { return searchtext; },
                ()=> { }, null, () => { return "Press " + Game.Options.General.Keybinds.Search.ToString().ToUpper() + " to search..."; })
                .TL_DeprecateMe(0, 0, AnchorType.MIN, AnchorType.MIN).BR_DeprecateMe(0, 60, AnchorType.MAX, AnchorType.MIN));
            AddChild(sc.TL_DeprecateMe(0, 60, AnchorType.MIN, AnchorType.MIN));
        }

        void Filter()
        {
            string f = searchtext.ToLower();
            sc.Filter((w) => ((DownloadCard)w).name.ToLower().Contains(f));
        }
    }
}
