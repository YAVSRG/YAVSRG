using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    public class Button : Widget
    {
        protected Sprite icon;
        protected string text;
        protected Action action;
        protected ColorFade color;

        public Button(string sprite, string label, Action onClick) : base()
        {
            icon = Content.LoadTextureFromAssets(sprite);
            text = label;
            action = onClick;
            color = new ColorFade(Game.Options.Theme.Base, Game.Options.Theme.Highlight);
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Draw(icon, left, top, right, bottom, color);
            SpriteBatch.DrawCentredText(text, 30f, (left + right) / 2, (top + bottom) / 2 - 20, Game.Options.Theme.MenuFont);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            color.Target = ScreenUtils.MouseOver(left, top, right, bottom) ? 1 : 0;
            color.Update();
            if (ScreenUtils.CheckButtonClick(left, top, right, bottom))
            {
                action();
            }
        }
    }
}
