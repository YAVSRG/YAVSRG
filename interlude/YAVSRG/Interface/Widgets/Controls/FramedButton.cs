using System;
using System.Drawing;
using Interlude.IO;
using Interlude.Interface.Animations;

namespace Interlude.Interface.Widgets
{
    class FramedButton : FrameContainer
    {
        AnimationColorMixer Fill, Border;
        public Func<bool> Highlight = () => false;
        protected Func<Bind> bind;
        public float FontSize = 20f;
        public string Tooltip = "";
        public string Tooltip2 = "";

        bool hover;

        public FramedButton(string label, Action onClick, Func<Bind> bind)
        {
            this.bind = bind;
            Animation.Add(Fill = new AnimationColorMixer(Game.Screens.DarkColor));
            Animation.Add(Border = new AnimationColorMixer(Game.Screens.HighlightColor));
            FrameColor = () => Border;
            BackColor = () => Fill;

            AddChild(new TextBox(() => label, AnchorType.CENTER, 0, true, () => Game.Options.Theme.MenuFont, () => Color.Black));

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

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            Fill.Target(Highlight() ? Game.Screens.BaseColor : Game.Screens.DarkColor);
            Border.Target(hover ? Game.Options.Theme.MenuFont : Game.Screens.HighlightColor);
            if (hover) { Game.Screens.Toolbar.SetTooltip(Tooltip + (bind != null ? "\n<" + bind().ToString() + ">" : ""), Tooltip2); }
        }
    }
}
