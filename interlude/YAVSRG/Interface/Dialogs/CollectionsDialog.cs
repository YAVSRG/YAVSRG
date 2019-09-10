using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Interface.Widgets;

namespace Interlude.Interface.Dialogs
{
    class CollectionsDialog : FadeDialog
    {
        //ChartSortingControls manages hotkeys to add/remove from collections
        public CollectionsDialog(Action<string> action) : base(action)
        {
            DropDown d = new DropDown((x) => { Game.Gameplay.Collections.SelectedCollection = x; }, () => Game.Gameplay.Collections.SelectedCollection, "Collection");

            void Refresh() { d.SetItems(Game.Gameplay.Collections.Collections.Keys.ToList()); }
            Refresh();
            AddChild(d.Reposition(-300, 0.5f, -50, 0.1f, -60, 0.5f, -10, 0.1f));
            AddChild(new SimpleButton("Create", () => { Game.Screens.AddDialog(new TextDialog("Enter name for collection: ",
                (s) => { if (s != "") { Game.Gameplay.Collections.SelectedCollection = s; Game.Gameplay.Collections.GetCollection(s); Refresh(); } })); }, () => false, null)
                .Reposition(50, 0.5f, -40, 0.5f, 250, 0.5f, 0, 0.5f));
            AddChild(new SimpleButton("Delete", () => { Game.Screens.AddDialog(new ConfirmDialog("Really delete this collection?",
                (s) => { if (s == "Y") { Game.Gameplay.Collections.DeleteCollection(Game.Gameplay.Collections.SelectedCollection); Refresh(); } })); }, () => false, null)
                .Reposition(50, 0.5f, 0, 0.5f, 250, 0.5f, 40, 0.5f));
        }
    }
}
