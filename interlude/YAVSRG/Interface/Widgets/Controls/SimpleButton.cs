using System;
using Interlude.Graphics;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    public class SimpleButton : Widget //planned new button to replace FramedButton in most things in future
    {
        protected Func<string> text;
        protected Animations.AnimationColorMixer color;
        protected Func<bool> highlight;
        protected Func<Bind> bind;
        public float FontSize = 20f;
        public string Tooltip = "";
        public string Tooltip2 = "";

        bool hover;

        public SimpleButton(string label, Action onClick, Func<bool> highlight, Func<Bind> bind) : this(() => label, onClick, highlight, bind)
        {

        }

        public SimpleButton(Func<string> label, Action onClick, Func<bool> highlight, Func<Bind> bind) : base()
        {
            this.bind = bind;
            text = label;
            this.highlight = highlight;
            Animation.Add(color = new Animations.AnimationColorMixer(Game.Screens.BaseColor));

            AddChild(new ClickableComponent()
            {
                OnClick = () =>
                {
                    Game.Audio.PlaySFX("click"); onClick();
                },
                Bind = bind,
                OnMouseOver = (b) => hover = b
            });
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawCentredText(text(), FontSize, bounds.CenterX, bounds.Top, color, true, Utils.ColorInterp(color, System.Drawing.Color.Black, 0.7f));
            SpriteBatch.DrawRect(bounds.SliceBottom(10), color);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            color.Target(highlight() ? System.Drawing.Color.White : hover ? Game.Screens.HighlightColor : Game.Screens.BaseColor);
            if (hover) { Game.Screens.Toolbar.SetTooltip(Tooltip + (bind != null ? "\n<" + bind().ToString() + ">" : ""), Tooltip2); }
        }
    }
}
