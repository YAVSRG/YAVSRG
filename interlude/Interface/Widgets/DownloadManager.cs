using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Net.Web;

namespace YAVSRG.Interface.Widgets
{
    class DownloadManager : ScrollContainer
    {
        public DownloadManager(EtternaPackData data) : base(10f, 10f, false)
        {
            PositionTopLeft(-510, 0, AnchorType.CENTER, AnchorType.MIN).PositionBottomRight(510, 0, AnchorType.CENTER, AnchorType.MAX);
            foreach (EtternaPackData.EtternaPack p in data.data)
            {
                AddChild(new DownloadCard(p).PositionBottomRight(1000, 50, AnchorType.MIN, AnchorType.MIN));
            }
        }
    }
}
