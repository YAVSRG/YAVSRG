using OpenTK;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Dialogs
{
    class TextDialog : Dialog
    {
        string prompt;
        string val = "";

        public TextDialog(string prompt, Action<string> action) : base(action) //todo: edit this to use the IME system
        {
            this.prompt = prompt;
            PositionTopLeft(-200, -100, AnchorType.CENTER, AnchorType.CENTER);
            PositionBottomRight(200, 100, AnchorType.CENTER, AnchorType.CENTER);
            Game.Instance.KeyPress += PressKey;
        }

        public void PressKey(object sender, KeyPressEventArgs e)
        {
            val += e.KeyChar;
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            if (Input.KeyTap(OpenTK.Input.Key.BackSpace) && val.Length > 0)
            {
                val = val.Remove(val.Length-1);
            }
            if (Input.KeyTap(OpenTK.Input.Key.Enter))
            {
                Game.Instance.KeyPress -= PressKey;
                Close(val);
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Font1.DrawCentredTextToFill(prompt, left, top, right, top + 100, Game.Options.Theme.MenuFont);
            SpriteBatch.Font2.DrawTextToFill(val, left, top + 100, right, bottom, Game.Options.Theme.MenuFont);
        }
    }
}
