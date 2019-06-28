using System;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    public class ClickableComponent : Widget
    {
        public Action OnClick;
        public Action OnRightClick;
        public Action<bool> OnMouseOver;
        public Func<Bind> Bind;

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
                if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
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
            if (Bind != null && Bind().Tapped())
            {
                OnClick?.Invoke();
            }
        }
    }
}
