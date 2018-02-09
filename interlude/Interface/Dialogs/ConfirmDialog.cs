using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Dialogs
{
    class ConfirmDialog : Dialog
    {
        string prompt;

        public ConfirmDialog(string prompt, Action<string> action) : base(action)
        {
            this.prompt = prompt;
            PositionTopLeft(-200, -100, AnchorType.CENTER, AnchorType.CENTER);
            PositionBottomRight(200, 100, AnchorType.CENTER, AnchorType.CENTER);
            Widgets.Add(new Button("buttonbase", "Yes", () => { Close("Y"); }).PositionTopLeft(0, 100, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.CENTER, AnchorType.MAX));
            Widgets.Add(new Button("buttonbase", "No", () => { Close("N"); }).PositionTopLeft(0, 100, AnchorType.CENTER, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.MAX, AnchorType.MAX));
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawCentredTextToFill(prompt, left, top, right, top + 100, Game.Options.Theme.MenuFont);
        }
    }
}
