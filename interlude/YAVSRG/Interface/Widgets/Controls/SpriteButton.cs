using System;
using Interlude.Graphics;
using Interlude.Interface.Animations;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    public class SpriteButton : Widget
    {
        protected string icon;
        protected AnimationColorMixer color;
        protected Func<Bind> bind;
        public string Tooltip = "";
        public string Tooltip2 = "";

        bool hover;

        public SpriteButton(string sprite, Action onClick, Func<Bind> bind) : base()
        {
            this.bind = bind;
            icon = sprite;
            Animation.Add(color = new AnimationColorMixer(Game.Screens.BaseColor));
            AddChild(new ClickableComponent()
            {
                Bind = bind,
                OnClick = () =>
                {
                    Game.Audio.PlaySFX("click"); onClick();
                },
                OnMouseOver = (b) => hover = b
            });
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Draw(icon, bounds, color);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            color.Target(hover ? Game.Screens.HighlightColor : Game.Screens.BaseColor);
            if (hover) { Game.Screens.Toolbar.SetTooltip(Tooltip + (bind != null ? "\n<" + bind().ToString() + ">" : ""), Tooltip2); }
        }
    }
}
