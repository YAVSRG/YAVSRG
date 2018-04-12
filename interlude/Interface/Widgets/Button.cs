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
        protected Animations.AnimationColorMixer color;

        public Button(string sprite, string label, Action onClick) : base()
        {
            icon = Content.LoadTextureFromAssets(sprite);
            text = label;
            action = onClick;
            Animation.Add(color = new Animations.AnimationColorMixer(Game.Screens.BaseColor));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Draw(icon, left, top, right, bottom, color);
            SpriteBatch.Font1.DrawCentredText(text, 30f, (left + right) / 2, (top + bottom) / 2 - 20, Game.Options.Theme.MenuFont);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            color.Target(ScreenUtils.MouseOver(left, top, right, bottom) ? Game.Screens.HighlightColor : Game.Screens.BaseColor);
            if (ScreenUtils.CheckButtonClick(left, top, right, bottom))
            {
                action();
            }
        }
    }
}
