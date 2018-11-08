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
        public EtternaPackData.EtternaPack item;
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

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float w = bounds.Width - 100;
            SpriteBatch.DrawRect(bounds, System.Drawing.Color.FromArgb(127, 0, 0, 0));
            //todo: move to textbox widgets
            SpriteBatch.Font1.DrawTextToFill(item.attributes.name, new Rect(bounds.Left, bounds.Top, bounds.Left + w * 0.7f, bounds.Bottom), Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedTextToFill(size, new Rect(bounds.Left + w * 0.75f, bounds.Top, bounds.Left + w * 0.9f, bounds.Bottom), Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawJustifiedTextToFill(difficulty, new Rect(bounds.Left + w * 0.9f, bounds.Top, bounds.Left + w, bounds.Bottom), Game.Options.Theme.MenuFont);
            if (item.attributes.download == "")
            {
                SpriteBatch.Font1.DrawCentredTextToFill("No DL Link", new Rect(bounds.Right - 100, bounds.Top, bounds.Right, bounds.Bottom), Game.Options.Theme.MenuFont);
            }
            ScreenUtils.DrawFrame(bounds, 30f, System.Drawing.Color.White);
        }

        private void Download()
        {
            Game.Tasks.AddTask(Charts.ChartLoader.DownloadAndImportPack(item.attributes.download, item.attributes.name), (b) => { }, "Downloading pack: " + item.attributes.name, true);
        }
    }
}
