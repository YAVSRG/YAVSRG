using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
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
            SpriteBatch.Font1.DrawText(get() != "" ? get() : text(), 20f, bounds.Left + 20, bounds.Top + 12.5f, color);
            ScreenUtils.DrawFrame(bounds, 30f, color);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            color.Target(focus ? Color.White : Game.Screens.HighlightColor);
            bounds = GetBounds(bounds);
            if (focus)
            {
                if (Input.KeyTap(Game.Options.General.Binds.Search, true) || !Input.HasIM() || ScreenUtils.CheckButtonClick(bounds))
                {
                    Input.ChangeIM(null);
                    focus = false;
                }
                else if (onSend != null && Input.KeyTap(OpenTK.Input.Key.Enter, true) && get() != "")
                {
                    onSend();
                    set("");
                    Input.ChangeIM(null);
                    focus = false;
                }
            }
            else
            {
                if (Input.KeyTap(Game.Options.General.Binds.Search) || ScreenUtils.CheckButtonClick(bounds))
                {
                    Input.ChangeIM(new InputMethod(set, get, update));
                    focus = true;
                }
            }
        }
    }
}
