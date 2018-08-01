using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Net.Web;

namespace YAVSRG.Interface.Widgets
{
    class DownloadCard : Widget
    {
        EtternaPackData.EtternaPack item;
        string size;
        string difficulty;

        public DownloadCard(EtternaPackData.EtternaPack data)
        {
            item = data;
            difficulty = Utils.RoundNumber(item.attributes.average / 2.5) + "*";
            size = Utils.RoundNumber(item.attributes.size / 1000000) + "MB";
            if (item.attributes.download != "")
            {
                AddChild(new SimpleButton("Download", Download, () => { return false; }, 20f)
                    .PositionTopLeft(100, 0, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(10, 0, AnchorType.MAX, AnchorType.MAX));
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float w = right - 100 - left;
            SpriteBatch.DrawRect(left, top, right, bottom, System.Drawing.Color.FromArgb(127, 0, 0, 0));
            SpriteBatch.Font1.DrawTextToFill(item.attributes.name, left, top, left + w * 0.7f, bottom, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedTextToFill(size, left + w * 0.75f, top, left + w * 0.9f, bottom, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedTextToFill(difficulty, left + w * 0.9f, top, left + w, bottom, Game.Options.Theme.MenuFont);
            if (item.attributes.download == "")
            {
                SpriteBatch.Font1.DrawCentredTextToFill("No DL Link", right - 100, top, right, bottom, Game.Options.Theme.MenuFont);
            }
            SpriteBatch.DrawFrame(left, top, right, bottom, 20f, System.Drawing.Color.White);
        }

        private void Download()
        {
            Charts.ChartLoader.TaskThreaded(() => { Charts.ChartLoader.DownloadAndImportPack(item.attributes.download, item.attributes.name); });
        }
    }
}
