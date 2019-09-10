using System;
using System.Collections.Generic;
using System.Linq;
using static Interlude.Gameplay.ChartLoader;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    public class ChartSortingControls : Widget
    {
        //todo: move collection controls out of here
        string selectedCollection = "Favourites";
        Widget sortControls, collectionControls;

        public ChartSortingControls() : base()
        {
            DropDown d = new DropDown((x) => { selectedCollection = x; }, () => selectedCollection, "Collection"); //referencable later so delete/create buttons can update list (nyi)
            AddChild(sortControls = new Widget());
            AddChild(collectionControls = new Widget());
            collectionControls.ToggleState();
            collectionControls.AddChild(d.SetItems(Game.Gameplay.Collections.Collections.Keys.ToList())
                .Reposition(-520, 1, -50, 1, -280, 1, -10, 1));

            collectionControls.AddChild(new SimpleButton("Create", () => { Game.Screens.AddDialog(new Dialogs.TextDialog("Enter name for collection: ", (s) => { if (s != "") { selectedCollection = s; } })); }, () => false, null)
                .TL_DeprecateMe(260, 50, AnchorType.MAX, AnchorType.MAX).BR_DeprecateMe(150, 10, AnchorType.MAX, AnchorType.MAX));
            collectionControls.AddChild(new SimpleButton("Delete", () => { Game.Screens.AddDialog(new Dialogs.ConfirmDialog("Really delete this collection?", (s) => { if (s == "Y") { Game.Gameplay.Collections.DeleteCollection(selectedCollection); } })); }, () => false, null)
                .TL_DeprecateMe(130, 50, AnchorType.MAX, AnchorType.MAX).BR_DeprecateMe(20, 10, AnchorType.MAX, AnchorType.MAX));

            sortControls.AddChild(new DropDown((x) => { Game.Options.Profile.ChartGroupMode = x; Refresh(); }, () => (Game.Options.Profile.ChartGroupMode), "Group by")
                .SetItems(GroupBy.Keys.ToList())
                .TL_DeprecateMe(680, 50, AnchorType.MAX, AnchorType.MAX).BR_DeprecateMe(480, 10, AnchorType.MAX, AnchorType.MAX));

            sortControls.AddChild(new DropDown((x) => { Game.Options.Profile.ChartSortMode = x; Refresh(); }, () => (Game.Options.Profile.ChartSortMode), "Sort by")
                .SetItems(SortBy.Keys.ToList())
                .TL_DeprecateMe(450, 50, AnchorType.MAX, AnchorType.MAX).BR_DeprecateMe(250, 10, AnchorType.MAX, AnchorType.MAX));

            sortControls.AddChild(new DropDown((x) => { Game.Options.Profile.ChartColorMode = x; Refresh(); }, () => (Game.Options.Profile.ChartColorMode), "Color by")
                .SetItems(ColorBy.Keys.ToList())
                .TL_DeprecateMe(220, 50, AnchorType.MAX, AnchorType.MAX).BR_DeprecateMe(20, 10, AnchorType.MAX, AnchorType.MAX));

            AddChild(new TextEntryBox((s) => { SearchString = s; }, () => SearchString, () => { Refresh(); }, null, () => ("Press " + Game.Options.General.Hotkeys.Search.ToString().ToUpper() + " to search..."))
                .Reposition(-600, 1, 10, 0, -20, 1, 70, 0));

            AddChild(new SpriteButton("buttoninfo", () => { collectionControls.ToggleState(); sortControls.ToggleState(); }, null) { Tooltip = "Collections (deprecate this button soon)" }
                .Reposition(-680, 1, 0, 0, -600, 1, 80, 0));
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            Game.Screens.DrawChartBackground(bounds, Game.Screens.DarkColor, 1.5f);
            ScreenUtils.DrawFrame(bounds.ExpandX(30), Game.Screens.HighlightColor);
            DrawWidgets(bounds);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Input.KeyPress(OpenTK.Input.Key.KeypadPlus))
            {
                Game.Gameplay.Collections.GetCollection(selectedCollection).AddItem(Game.Gameplay.CurrentCachedChart);
            }
            else if (Input.KeyPress(OpenTK.Input.Key.KeypadMinus))
            {
                Game.Gameplay.Collections.GetCollection(selectedCollection).RemoveItem(Game.Gameplay.CurrentCachedChart);
            }
        }
    }
}
