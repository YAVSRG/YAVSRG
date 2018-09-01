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
            this.label = label;
            Change(start, set);
        }

        public void Change(Key start, Action<Key> set)
        {
            bind = start;
            this.set = set;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            //todo: reduce duplication between this and the switchers
            //slice
            SpriteBatch.DrawRect(new Rect(bounds.Left, bounds.Top, bounds.Left + 20, bounds.Bottom), Game.Screens.BaseColor);
            SpriteBatch.DrawRect(new Rect(bounds.Right - 20, bounds.Top, bounds.Right, bounds.Bottom), Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredText(bind.ToString(), 30, bounds.CenterX, bounds.Top, listening ? System.Drawing.Color.Fuchsia : Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawCentredText(label, 20, bounds.CenterX, bounds.Top - 30, Game.Options.Theme.MenuFont);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (!listening && ScreenUtils.CheckButtonClick(bounds))
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
