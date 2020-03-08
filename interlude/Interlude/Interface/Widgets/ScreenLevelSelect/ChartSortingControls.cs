using System.Linq;
using Prelude.Utilities;
using Interlude.IO;
using static Interlude.Gameplay.ChartLoader;

namespace Interlude.Interface.Widgets
{
    public class ChartSortingControls : Widget
    {
        public ChartSortingControls() : base()
        {
            AddChild(new DropDown((x) => { Game.Options.Profile.ChartGroupMode = x; Refresh(); }, () => (Game.Options.Profile.ChartGroupMode), "Group by")
                .SetItems(GroupBy.Keys.ToList())
                .Reposition(-680, 1, -50, 1, -480, 1, -10, 1));

            AddChild(new DropDown((x) => { Game.Options.Profile.ChartSortMode = x; Refresh(); }, () => (Game.Options.Profile.ChartSortMode), "Sort by")
                .SetItems(SortBy.Keys.ToList())
                .Reposition(-450, 1, -50, 1, -250, 1, -10, 1));

            AddChild(new DropDown((x) => { Game.Options.Profile.ChartColorMode = x; Refresh(); }, () => (Game.Options.Profile.ChartColorMode), "Color by")
                .SetItems(ColorBy.Keys.ToList())
                .Reposition(-220, 1, -50, 1, -20, 1, -10, 1));

            AddChild(new TextEntryBox((s) => { SearchString = s; }, () => SearchString, () => { Refresh(); }, null, () => ("Press " + Game.Options.General.Hotkeys.Search.ToString().ToUpper() + " to search..."))
                .Reposition(-600, 1, 10, 0, -20, 1, 70, 0));
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
            if (Game.Options.General.Hotkeys.AddToCollection.Tapped())
            {
                Game.Gameplay.Collections.GetCollection(Game.Gameplay.Collections.SelectedCollection).AddItem(Game.Gameplay.CurrentCachedChart);
                Logging.Log("Added to '" + Game.Gameplay.Collections.SelectedCollection + "'");
            }
            else if (Game.Options.General.Hotkeys.RemoveFromCollection.Tapped())
            {
                Game.Gameplay.Collections.GetCollection(Game.Gameplay.Collections.SelectedCollection).RemoveItem(Game.Gameplay.CurrentCachedChart);
                Logging.Log("Removed from '" + Game.Gameplay.Collections.SelectedCollection + "'");
            }
        }
    }
}
