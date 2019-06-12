using System;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    public class ClickableComponent : Widget
    {
        public Action OnClick;
        public Action OnRightClick;
        public Action<bool> OnMouseOver;
        public Func<OpenTK.Input.Key> Bind;

        bool hover;

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);

            if (ScreenUtils.MouseOver(bounds))
            {
                if (!hover)
                {
                    hover = true;
                    OnMouseOver?.Invoke(true);
                }
                if (Input.MouseClick(OpenTK.Input.MouseButton.Left) || (Bind != null && Input.KeyTap(Bind())))
                {
                    OnClick?.Invoke();
                }
                else if (Input.MouseClick(OpenTK.Input.MouseButton.Right))
                {
                    OnRightClick?.Invoke();
                }
            }
            else
            {
                if (hover)
                {
                    hover = false;
                    OnMouseOver?.Invoke(false);
                }
            }
        }
    }
}
