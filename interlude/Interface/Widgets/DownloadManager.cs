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
            AddChild(new TextEntryBox((s) => { searchtext = s; }, () => { return searchtext; }, Filter).PositionTopLeft(0,0,AnchorType.MIN,AnchorType.MIN).PositionBottomRight(0,55,AnchorType.MAX,AnchorType.MIN));
            AddChild(sc.PositionTopLeft(0, 55, AnchorType.MIN, AnchorType.MIN));
        }

        void Filter()
        {
            string f = searchtext.ToLower();
            foreach (Widget w in sc.Items())
            {
                if (!((DownloadCard)w).item.attributes.name.ToLower().Contains(f))
                {
                    w.State = 0;
                }
                else
                {
                    w.State = 1;
                }
            }
        }
    }
}
