using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Charts.ChartLoader;
using OpenTK;

namespace YAVSRG.Interface.Widgets
{
    public class ChartSortingControls : Widget
    {
        string selectedCollection = "Favourites";
        Widget sortControls, collectionControls;

        public ChartSortingControls() : base()
        {
            DropDown d = new DropDown((x) => { selectedCollection = x; }, () => (selectedCollection), "Collection"); //referencable later so delete/create buttons can update list (nyi)
            AddChild(sortControls = new Widget());
            AddChild(collectionControls = new Widget());
            collectionControls.ToggleState();
            collectionControls.AddChild(d.SetItems(Game.Gameplay.Collections.Collections.Keys.ToList())
                .PositionTopLeft(520, 50, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(280, 10, AnchorType.MAX, AnchorType.MAX));

            collectionControls.AddChild(new SimpleButton("Create", () => { Game.Screens.AddDialog(new Dialogs.TextDialog("Enter name for collection: ", (s) => { if (s != "") { selectedCollection = s; } })); }, () => (false), 20f)
                .PositionTopLeft(260, 50, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(150, 10, AnchorType.MAX, AnchorType.MAX));
            collectionControls.AddChild(new SimpleButton("Delete", () => { Game.Screens.AddDialog(new Dialogs.ConfirmDialog("Really delete this collection?", (s) => { if (s == "Y") { Game.Gameplay.Collections.DeleteCollection(selectedCollection); } })); }, () => (false), 20f)
                .PositionTopLeft(130, 50, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(20, 10, AnchorType.MAX, AnchorType.MAX));

            sortControls.AddChild(new DropDown((x) => { Game.Options.Profile.ChartGroupMode = x; Refresh(); }, () => (Game.Options.Profile.ChartGroupMode), "Group by")
                .SetItems(GroupBy.Keys.ToList())
                .PositionTopLeft(520, 50, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(280, 10, AnchorType.MAX, AnchorType.MAX));

            sortControls.AddChild(new DropDown((x) => { Game.Options.Profile.ChartSortMode = x; Refresh(); }, () => (Game.Options.Profile.ChartSortMode), "Sort by")
                .SetItems(SortBy.Keys.ToList())
                .PositionTopLeft(260, 50, AnchorType.MAX, AnchorType.MAX).PositionBottomRight(20, 10, AnchorType.MAX, AnchorType.MAX));

            AddChild(new TextEntryBox((s) => { SearchString = s; }, () => (SearchString), () => { Refresh(); }, null, () => ("Press " + Game.Options.General.Binds.Search.ToString().ToUpper() + " to search..."))
                .PositionTopLeft(520, 10, AnchorType.MAX, AnchorType.MIN).PositionBottomRight(20, 70, AnchorType.MAX, AnchorType.MIN));

            AddChild(new SpriteButton("buttoninfo", "Collections", () => { collectionControls.ToggleState(); sortControls.ToggleState(); }).PositionTopLeft(600, 0, AnchorType.MAX,AnchorType.MIN).PositionBottomRight(520,80,AnchorType.MAX,AnchorType.MIN));
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            Game.Screens.DrawChartBackground(bounds, Game.Screens.DarkColor, 1.5f);
            ScreenUtils.DrawFrame(bounds.ExpandX(30), 30f, Game.Screens.HighlightColor);
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
