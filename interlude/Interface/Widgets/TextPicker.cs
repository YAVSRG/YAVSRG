using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class TextPicker : Widget
    {
        string[] options;
        int selection;
        Action<int> set;
        string label;

        public TextPicker(string label, string[] options, int start, Action<int> set) : base()
        {
            this.label = label;
            this.set = set;
            this.options = options;
            selection = start;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left,top,right,bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, left + 20, bottom, Game.Screens.BaseColor);
            SpriteBatch.DrawRect(right - 20, top, right, bottom, Game.Screens.BaseColor);
            SpriteBatch.DrawCentredText(options[selection], 30, (left+right)/2, top, Game.Options.Theme.MenuFont);
            SpriteBatch.DrawCentredText(label, 20, (left+right)/2, top - 30, Game.Options.Theme.MenuFont);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left,top,right,bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (ScreenUtils.CheckButtonClick(left, top, left + 20, bottom))
            {
                selection = Utils.Modulus(selection - 1, options.Length);
                set(selection);
            }
            else if (ScreenUtils.CheckButtonClick(right - 20, top, right, bottom))
            {
                selection = Utils.Modulus(selection + 1, options.Length);
                set(selection);
            }
        }
    }
}
