using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Interface.Widgets
{
    class ThemePanel : Widget
    {
        public ThemePanel()
        {
            AddChild(new SimpleButton("Change Theme", () => { Game.Screens.AddDialog(new Dialogs.ThemeSelectDialog((s) => { })); }, () => false, null).Reposition(200, 0.5f, 525, 0, 500, 0.5f, 575, 0));
            AddChild(new SimpleButton("Create Theme", () =>
            {
                Game.Screens.AddDialog(new Dialogs.TextDialog("Enter theme name: ", (s) =>
                {
                    if (s != "")
                    {
                        Game.Options.Themes.CreateNewTheme(s);
                        System.Diagnostics.Process.Start("file://" + System.IO.Path.GetFullPath(Options.Themes.ThemeManager.AssetsDir));
                    }
                }));
            }, () => false, null).Reposition(200, 0.5f, 625, 0, 500, 0.5f, 675, 0));
        }
    }
}
