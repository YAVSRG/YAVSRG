using System.Drawing;
using OpenTK.Input;
using Prelude.Utilities;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class KeyBinder : Widget
    {
        public bool AllowAltBinds = true;
        SetterGetter<Bind> Value;
        string Label;
        bool listening, ctrl, shift;

        public KeyBinder(string label, SetterGetter<Bind> value) : base()
        {
            Label = label;
            Value = value;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(120, Game.Screens.DarkColor));
            ;
            SpriteBatch.Font1.DrawCentredText(((Bind)Value).ToString(), 30, bounds.CenterX, bounds.Top,
                listening ? Color.Fuchsia : 
                (((Bind)Value).Held() ? Game.Screens.HighlightColor : Game.Options.Theme.MenuFont), true, Game.Screens.DarkColor);

            SpriteBatch.Font2.DrawCentredText(Label, 20, bounds.CenterX, bounds.Top - 30, Game.Options.Theme.MenuFont);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (!listening && ScreenUtils.CheckButtonClick(bounds))
            {
                listening = true;
                ctrl = false;
                shift = false;
                Game.Instance.KeyDown += OnKeyPress;
            }
        }

        private void OnKeyPress(object o, KeyboardKeyEventArgs k)
        {
            if (k.Key == Key.ControlLeft || k.Key == Key.ControlRight)
            {
                ctrl = true && AllowAltBinds;
            }
            else if (k.Key == Key.ShiftRight || k.Key == Key.ShiftLeft)
            {
                shift = true && AllowAltBinds;
            }
            else
            {
                Value.Set(new KeyBind(k.Key));
                if (shift || ctrl) { Value.Set(new AltBind(Value, shift, ctrl)); }
                listening = false;
                Game.Instance.KeyDown -= OnKeyPress;
            }
        }
    }
}
