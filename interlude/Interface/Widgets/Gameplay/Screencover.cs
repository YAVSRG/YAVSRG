using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    public class Screencover : GameplayWidget
    {
        bool down;
        Sprite texture;

        public Screencover(YAVSRG.Gameplay.ScoreTracker st, bool d) : base(st)
        {
            down = d;
            texture = Content.LoadTextureFromAssets("screencover");
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (down)
            {
                SpriteBatch.Draw(texture, left, bottom - Game.Options.Theme.ColumnWidth, right, bottom, System.Drawing.Color.White, 0, 0, 2);
                SpriteBatch.Draw(texture, left, bottom - Game.Options.Theme.ColumnWidth, right, top, System.Drawing.Color.White, 0, 1);
            }
            else
            {
                SpriteBatch.Draw(texture, left, top + Game.Options.Theme.ColumnWidth, right, top, System.Drawing.Color.White, 0, 0, 2);
                SpriteBatch.Draw(texture, left, top + Game.Options.Theme.ColumnWidth, right, bottom, System.Drawing.Color.White, 0, 1);
            }
            //SpriteBatch.Draw(texture, left, h * amount - COLUMNWIDTH - Height, right, h * amount - Height, Color.White, 0, 0, 2);
            //SpriteBatch.Draw(texture, left, -Height, right, h * amount - COLUMNWIDTH - Height, Color.White, 0, 1, 2);
        }
    }
}
