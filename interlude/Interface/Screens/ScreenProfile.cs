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
        public override void Draw(float left, float top, float right, float bottom)
        {
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Font1.DrawCentredTextToFill(Game.Options.Profile.Name, left, top, right, top + 100, Game.Options.Theme.MenuFont);
            if (Game.Options.Profile.Stats.Scores[1] == null)
            {
                SpriteBatch.Font1.DrawCentredTextToFill("You have no (4k) scores", left + 100, top + 100, right - 100, bottom - 100, Color.White);
            }
            else
            {
                int c = Math.Min(Game.Options.Profile.Stats.Scores[1].Count, 25);
                for (int i = 0; i < c; i++)
                {
                    TopScore s = Game.Options.Profile.Stats.Scores[1][i];
                    SpriteBatch.DrawRect(left, top + 100 + 30 * i, right, top + 130 + 30 * i, Color.FromArgb(50, i % 2 == 0 ? Color.Gray : Color.Black));
                    
                    SpriteBatch.Font2.DrawText(Charts.ChartLoader.Cache.Charts.ContainsKey(s.abspath) ? Charts.ChartLoader.Cache.Charts[s.abspath].title : "THE DATA IS MISSING", 24f, left + 10, top + 100 + 30 * i, Game.Options.Theme.MenuFont);
                    SpriteBatch.Font2.DrawText(s.mods, 24f, left + 800, top + 100 + 30 * i, Game.Options.Theme.MenuFont);
                    SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(s.accuracy) + "%", 24f, right - 100, top + 100 + 30 * i, Game.Options.Theme.MenuFont);
                    SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(s.rating), 24f, right - 10, top + 100 + 30 * i, Game.Options.Theme.MenuFont);
                }
            }
        }
    }
}
