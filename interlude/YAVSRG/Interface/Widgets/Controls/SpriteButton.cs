using System;
using Interlude.Graphics;
using Interlude.Interface.Animations;

namespace Interlude.Interface.Widgets
{
    public class SpriteButton : Widget
    {
        protected string icon;
        protected string text;
        protected AnimationColorMixer color;

        bool hover;

        public SpriteButton(string sprite, string label, Action onClick) : base()
        {
            icon = sprite;
            text = label;
            Animation.Add(color = new AnimationColorMixer(Game.Screens.BaseColor));
            AddChild(new ClickableComponent()
            {
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
            if (hover) Game.Screens.Toolbar.SetTooltip(text, "");
        }
    }
}
