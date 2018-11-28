using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Net.Web;

namespace YAVSRG.Interface.Widgets
{
    class DownloadManager : Widget
    {
        string searchtext = "";
        ScrollContainer sc;

        public DownloadManager(EtternaPackData data)
        {
            sc = new ScrollContainer(10f, 10f, false);
            PositionTopLeft(0, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(-620, 0, AnchorType.MAX, AnchorType.MAX);
            foreach (EtternaPackData.EtternaPack p in data.data)
            {
                sc.AddChild(new DownloadCard(p).PositionBottomRight(600, 50, AnchorType.MIN, AnchorType.MIN));
            }
            AddChild(new TextEntryBox((s) => { searchtext = s; }, () => { return searchtext; },
                Filter, null, () => { return "Press " + Game.Options.General.Binds.Search.ToString().ToUpper() + " to search..."; })
                .PositionTopLeft(0,0,AnchorType.MIN,AnchorType.MIN).PositionBottomRight(0,60,AnchorType.MAX,AnchorType.MIN));
            AddChild(sc.PositionTopLeft(0, 60, AnchorType.MIN, AnchorType.MIN));
        }

        public DownloadManager(List<BloodcatChartData> data)
        {
            sc = new ScrollContainer(10f, 10f, false);
            PositionTopLeft(0, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(-620, 0, AnchorType.MAX, AnchorType.MAX);
            foreach (BloodcatChartData p in data)
            {
                sc.AddChild(new DownloadCard(p).PositionBottomRight(600, 50, AnchorType.MIN, AnchorType.MIN));
            }
            AddChild(new TextEntryBox((s) => { searchtext = s; }, () => { return searchtext; },
                ()=> { }, null, () => { return "Press " + Game.Options.General.Binds.Search.ToString().ToUpper() + " to search..."; })
                .PositionTopLeft(0, 0, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(0, 60, AnchorType.MAX, AnchorType.MIN));
            AddChild(sc.PositionTopLeft(0, 60, AnchorType.MIN, AnchorType.MIN));
        }

        void Filter()
        {
            string f = searchtext.ToLower();
            foreach (Widget w in sc.Items())
            {
                if (!((DownloadCard)w).name.ToLower().Contains(f))
                {
                    w.SetState(WidgetState.DISABLED);
                }
                else
                {
                    w.SetState(WidgetState.NORMAL);
                }
            }
        }
    }
}
