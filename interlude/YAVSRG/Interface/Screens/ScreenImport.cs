using System;
using OpenTK.Input;
using Interlude.Gameplay;
using Interlude.Graphics;

namespace Interlude.Interface.Screens
{
    class ScreenImport : Screen
    {
        public ScreenImport() : base()
        {
            AddChild(new Widgets.SimpleButton("Import from osu!", () =>
            {
                Game.Screens.AddDialog(new Dialogs.ConfirmDialog("If you already imported and modified charts, background images or audio from osu!, they will be overwritten. Continue?", (s) =>
                {
                    if (s == "Y")
                    {
                        Game.Tasks.AddTask(ChartLoader.ImportOsu(), (b) => { }, "Import from osu!", false);
                    }
                }));
            },
            () => { return false; }, null)
            { FontSize = 40 }
            .TL_DeprecateMe(0,100,AnchorType.MIN,AnchorType.MAX).BR_DeprecateMe(0,0,AnchorType.CENTER,AnchorType.MAX));
            AddChild(new Widgets.SimpleButton("Import from Stepmania/Etterna", () =>
            {
                Game.Screens.AddDialog(new Dialogs.ConfirmDialog("If you already imported and modified charts, background images or audio, they will be overwritten. Continue?", (s) =>
                {
                    if (s == "Y")
                    {
                        Game.Tasks.AddTask(ChartLoader.ImportStepmania(), (b) => { }, "Import from Stepmania/Etterna", false);
                    }
                }));
            },
            () => { return false; }, null)
            { FontSize = 40 }
            .TL_DeprecateMe(0, 100, AnchorType.CENTER, AnchorType.MAX).BR_DeprecateMe(0, 0, AnchorType.MAX, AnchorType.MAX));
            Net.Web.WebUtils.DownloadJsonObject<Net.Web.EtternaPackData>("https://api.etternaonline.com/v2/packs/", (d) => { RightAnchor.Move(-620); AddChild(new Widgets.DownloadManager(d)); });
            //Net.Web.WebUtils.DownloadJsonObject<List<Net.Web.BloodcatChartData>>("https://bloodcat.com/osu/?m=3&mod=json&q=put_query_here", (d) => { BottomRight.Target(620, 0); AddChild(new Widgets.DownloadManager(d)); });
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Instance.FileDrop += HandleFileDrop;
            Game.Screens.Toolbar.Icons.Filter(0b00010111);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Instance.FileDrop -= HandleFileDrop;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawCentredTextToFill("Drag and drop a file or folder to import it.", new Rect(bounds.Left, bounds.Top + 100, bounds.Right, bounds.Top + 200), Game.Options.Theme.MenuFont);
            //if (Charts.ChartLoader.LastStatus != Charts.ChartLoader.ChartLoadingStatus.InProgress)
            //SpriteBatch.Font1.DrawCentredTextToFill(Charts.ChartLoader.LastOutput, new Rect(bounds.Left, -300, bounds.Right, 300), Game.Options.Theme.MenuFont);
        }

        protected void HandleFileDrop(object sender, FileDropEventArgs e)
        {
            string s = e.FileName;
            Game.Tasks.AddTask(ChartLoader.AutoImportFromPath(s), (b) => { }, "Import from " + System.IO.Path.GetFileName(e.FileName), true);
        }
    }
}
