using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using System.Drawing;

namespace YAVSRG.Interface.Screens
{
    class ScreenProfile : Screen
    {
        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            //slice
            SpriteBatch.Font1.DrawCentredTextToFill(Game.Options.Profile.Name, new Rect(bounds.Left, bounds.Top, bounds.Right, bounds.Top + 100), Game.Options.Theme.MenuFont);
            if (Game.Options.Profile.Stats.Scores[1] == null)
            {
                SpriteBatch.Font1.DrawCentredTextToFill("You have no (4k) scores", bounds.Expand(-100,-100), Color.White);
            }
            else
            {
                int c = Math.Min(Game.Options.Profile.Stats.Scores[1].Count, 25);
                for (int i = 0; i < c; i++)
                {
                    TopScore s = Game.Options.Profile.Stats.Scores[1][i];
                    SpriteBatch.DrawRect(new Rect(bounds.Left, bounds.Top + 100 + 30 * i, bounds.Right, bounds.Top + 130 + 30 * i), Color.FromArgb(50, i % 2 == 0 ? Color.Gray : Color.Black));
                    
                    SpriteBatch.Font2.DrawText(Charts.ChartLoader.Cache.Charts.ContainsKey(s.abspath) ? Charts.ChartLoader.Cache.Charts[s.abspath].title : "THE DATA IS MISSING", 24f, bounds.Left + 10, bounds.Top + 100 + 30 * i, Game.Options.Theme.MenuFont);
                    SpriteBatch.Font2.DrawText(s.mods, 24f, bounds.Left + 800, bounds.Top + 100 + 30 * i, Game.Options.Theme.MenuFont);
                    SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(s.accuracy) + "%", 24f, bounds.Right - 100, bounds.Top + 100 + 30 * i, Game.Options.Theme.MenuFont);
                    SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(s.rating), 24f, bounds.Right - 10, bounds.Top + 100 + 30 * i, Game.Options.Theme.MenuFont);
                }
            }
        }
    }
}
