using System;
using System.Drawing;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    class TextEntryBox : Widget
    {
        bool focus;
        Animations.AnimationColorMixer color;

        Action<string> set;
        Func<string> get;
        Action update;
        Func<string> text;
        Action onSend;

        public TextEntryBox(Action<string> setter, Func<string> getter, Action updater, Action send, Func<string> label)
        {
            set = setter;
            get = getter;
            update = updater;
            onSend = send;
            text = label;
            Animation.Add(color = new Animations.AnimationColorMixer(Game.Screens.HighlightColor));
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds, Game.Screens.DarkColor);
            SpriteBatch.Font1.DrawText(get() != "" ? get() : text(), 20f, bounds.Left + 20, bounds.Top + 12.5f, color, true, Game.Screens.DarkColor);
            ScreenUtils.DrawFrame(bounds, color);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            color.Target(focus ? Color.White : Game.Screens.HighlightColor);
            bounds = GetBounds(bounds);
            if (focus)
            {
                if (Game.Options.General.Hotkeys.Search.Tapped(true) || !Input.HasIM() || ScreenUtils.CheckButtonClick(bounds))
                {
                    Input.ChangeIM(null);
                    focus = false;
                }
                else if (onSend != null && Game.Options.General.Hotkeys.Select.Tapped(true) && get() != "")
                {
                    onSend();
                    set("");
                    Input.ChangeIM(null);
                    focus = false;
                }
            }
            else
            {
                if (Game.Options.General.Hotkeys.Search.Tapped() || ScreenUtils.CheckButtonClick(bounds))
                {
                    Input.ChangeIM(new InputMethod(set, get, update));
                    focus = true;
                }
            }
        }
    }
}
