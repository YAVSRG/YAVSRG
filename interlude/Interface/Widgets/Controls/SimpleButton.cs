using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class SimpleButton : Widget //planned new button to replace FramedButton in most things in future
    {
        protected string text;
        protected Action action;
        protected Animations.AnimationColorMixer color;
        protected Func<bool> highlight;
        protected float fontsize;

        public SimpleButton(string label, Action onClick, Func<bool> highlight, float size) : base()
        {
            fontsize = size;
            text = label;
            action = onClick;
            this.highlight = highlight;
            Animation.Add(color = new Animations.AnimationColorMixer(Game.Screens.BaseColor));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Font1.DrawCentredText(text, fontsize, (left + right) / 2, top, color);
            SpriteBatch.DrawRect(left, bottom - 10, right, bottom, color);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            color.Target(highlight() ? System.Drawing.Color.White :ScreenUtils.MouseOver(left, top, right, bottom) ? Game.Screens.HighlightColor : Game.Screens.BaseColor);
            if (ScreenUtils.CheckButtonClick(left, top, right, bottom))
            {
                action();
            }
        }
    }
}
