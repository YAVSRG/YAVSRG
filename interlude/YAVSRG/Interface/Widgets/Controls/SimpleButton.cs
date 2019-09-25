using System;
using Interlude.Graphics;
using Interlude.IO;
using Interlude.Interface.Animations;

namespace Interlude.Interface.Widgets
{
    public class SimpleButton : Widget //planned new button to replace FramedButton in most things in future
    {
        protected Func<string> text;
        protected AnimationColorMixer color;
        protected AnimationSlider hoverAnimation;
        protected Func<bool> highlight;
        protected Func<Bind> bind;
        public float FontSize = 20f;
        public string Tooltip = "";
        public string Tooltip2 = "";

        public SimpleButton(string label, Action onClick, Func<bool> highlight, Func<Bind> bind) : this(() => label, onClick, highlight, bind)
        {

        }

        public SimpleButton(Func<string> label, Action onClick, Func<bool> highlight, Func<Bind> bind) : base()
        {
            this.bind = bind;
            text = label;
            this.highlight = highlight;
            Animation.Add(color = new AnimationColorMixer(Game.Screens.BaseColor));
            Animation.Add(hoverAnimation = new AnimationSlider(0));

            AddChild(new ClickableComponent()
            {
                OnClick = () =>
                {
                    Game.Audio.PlaySFX("click"); onClick();
                },
                Bind = bind,
                OnMouseOver = (b) => hoverAnimation.Target = b ? 1 : 0
            });
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawCentredText(text(), FontSize, bounds.CenterX, bounds.Top, color, true, Utils.ColorInterp(color, System.Drawing.Color.Black, 0.7f));
            float w = Math.Min(highlight() ? 0.5f : 1f, 1 - hoverAnimation) * bounds.Width * -0.5f;
            SpriteBatch.DrawRect(bounds.SliceBottom(10), System.Drawing.Color.FromArgb(180, color));
            SpriteBatch.DrawRect(bounds.SliceBottom(10).ExpandX(w), color);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            color.Target(highlight() ? System.Drawing.Color.White : hoverAnimation.Target == 1 ? Game.Screens.HighlightColor : Game.Screens.BaseColor);
            if (hoverAnimation.Target == 1) { Game.Screens.SetTooltip(Tooltip + (bind != null ? "\n<" + bind().ToString() + ">" : ""), Tooltip2); }
        }
    }
}
