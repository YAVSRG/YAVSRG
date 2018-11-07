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

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds.SliceLeft(20), Game.Screens.BaseColor);
            SpriteBatch.DrawRect(bounds.SliceRight(20), Game.Screens.BaseColor);
            SpriteBatch.Font1.DrawCentredText(options[selection], 30, bounds.CenterX, bounds.Top, Game.Options.Theme.MenuFont, true, Game.Screens.BaseColor);
            SpriteBatch.Font2.DrawCentredText(label, 20, bounds.CenterX, bounds.Top - 30, Game.Options.Theme.MenuFont);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            //todo: rectangle slicing methods
            if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left, bounds.Top, bounds.Left + 20, bounds.Bottom)))
            {
                selection = Utils.Modulus(selection - 1, options.Length);
                set(selection);
            }
            else if (ScreenUtils.CheckButtonClick(new Rect(bounds.Right - 20, bounds.Top, bounds.Right, bounds.Bottom)))
            {
                selection = Utils.Modulus(selection + 1, options.Length);
                set(selection);
            }
        }
    }
}
