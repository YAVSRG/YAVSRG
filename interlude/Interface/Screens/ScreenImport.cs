using OpenTK.Input;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Screens
{
    class ScreenImport : Screen
    {
        public ScreenImport() : base()
        {
            AddChild(new Widgets.SimpleButton("Import from osu!", () =>
            {
                Game.Screens.AddDialog(new Dialogs.ConfirmDialog("If you already imported and modified charts, background images or audio from osu!, they will be overwritten. This will take a while. Continue?", (s) =>
                {
                    if (s == "Y")
                    {
                        Charts.ChartLoader.TaskThreaded(() => { Charts.ChartLoader.ImportOsu(); }, "Import from osu!");
                    }
                }));
            },
            () => { return false; }, 40f)
            .PositionTopLeft(0,100,AnchorType.MIN,AnchorType.MAX).PositionBottomRight(0,0,AnchorType.CENTER,AnchorType.MAX));
            AddChild(new Widgets.SimpleButton("Import from Stepmania/Etterna", () =>
            {
                Game.Screens.AddDialog(new Dialogs.ConfirmDialog("If you already imported and modified charts, background images or audio, they will be overwritten. This will take a while. Continue?", (s) =>
                {
                    if (s == "Y")
                    {
                        Charts.ChartLoader.TaskThreaded(() => { Charts.ChartLoader.ImportStepmania(); }, "Import from Stepmania/Etterna");
                    }
                }));
            },
            () => { return false; }, 40f)
            .PositionTopLeft(0, 100, AnchorType.CENTER, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
            Net.Web.WebUtils.DownloadJsonObject<Net.Web.EtternaPackData>("https://api.etternaonline.com/v2/packs/", (d) => { B.Target(620, 0); AddChild(new Widgets.DownloadManager(d)); });
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            Game.Instance.FileDrop += HandleFileDrop;
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Instance.FileDrop -= HandleFileDrop;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Font1.DrawCentredTextToFill("Drag and drop a file or folder to import it.", left, top + 100, right, top + 200, Game.Options.Theme.MenuFont);
            //if (Charts.ChartLoader.LastStatus != Charts.ChartLoader.ChartLoadingStatus.InProgress)
            SpriteBatch.Font1.DrawCentredTextToFill(Charts.ChartLoader.LastOutput, left, -300, right, 300, Game.Options.Theme.MenuFont);
        }

        protected void HandleFileDrop(object sender, FileDropEventArgs e)
        {
            string s = e.FileName;
            Charts.ChartLoader.TaskThreaded(() => { Charts.ChartLoader.AutoImportFromPath(s); }, "Import from " + System.IO.Path.GetFileName(e.FileName));
        }
    }
}
