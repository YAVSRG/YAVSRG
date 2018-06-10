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

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawTilingTexture(icon, left, top, right, bottom, 200, scroll, 0, color);
            SpriteBatch.DrawFrame(left, top, right, bottom, 30f, System.Drawing.Color.White);
            SpriteBatch.Font1.DrawCentredText(text, 30f, (left + right) / 2, (top + bottom) / 2 - 20, Game.Options.Theme.MenuFont);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            scroll += 0.002f;
        }
    }
}
