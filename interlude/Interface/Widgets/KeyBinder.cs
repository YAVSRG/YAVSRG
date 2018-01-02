using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using OpenTK.Input;

namespace YAVSRG.Interface.Widgets
{
    public class KeyBinder : Widget
    {
        Key bind;
        Action<Key> set;
        string label;
        bool listening;

        public KeyBinder(string label, Key start, Action<Key> set) : base()
        {
            bind = start;
            this.label = label;
            this.set = set;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, left + 20, bottom, Game.Options.Theme.Base);
            SpriteBatch.DrawRect(right - 20, top, right, bottom, Game.Options.Theme.Base);
            SpriteBatch.DrawCentredText(bind.ToString(), 30, (left + right) / 2, top, listening ? System.Drawing.Color.Fuchsia : Game.Options.Theme.MenuFont);
            SpriteBatch.DrawCentredText(label, 20, (left + right) / 2, top - 30, Game.Options.Theme.MenuFont);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (!listening && ScreenUtils.CheckButtonClick(left, top, right, bottom))
            {
                listening = true;
                Game.Instance.KeyDown += OnKeyPress;
            }
        }

        private void OnKeyPress(object o, KeyboardKeyEventArgs k)
        {
            bind = k.Key;
            set(bind);
            listening = false;
            Game.Instance.KeyDown -= OnKeyPress;
        }
    }
}
