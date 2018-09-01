using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    class FramedButton : Button
    {
        float scroll;

        public FramedButton(string sprite, string label, Action onClick) : base(sprite, label, onClick)
        {
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawTilingTexture(icon, bounds, 200, scroll, 0, color);
            ScreenUtils.DrawFrame(bounds, 30f, System.Drawing.Color.White);
            SpriteBatch.Font1.DrawCentredText(text, 30f, bounds.CenterX, bounds.CenterY - 20, Game.Options.Theme.MenuFont);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            scroll += 0.002f;
        }
    }
}
