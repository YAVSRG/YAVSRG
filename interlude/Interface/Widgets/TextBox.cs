using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class TextBox : Widget
    {
        Func<string> text;
        AnchorType style;
        bool font;
        bool fill;
        Animations.AnimationColorMixer color;

        public TextBox(string text, AnchorType position, bool useFill, bool altFont, System.Drawing.Color c) : this(() => { return text; }, position, useFill, altFont, c) { }

        public TextBox(Func<string> text, AnchorType position, bool useFill, bool altFont, System.Drawing.Color c)
        {
            this.text = text;
            style = position;
            font = altFont;
            fill = useFill;
            Animation.Add(color = new Animations.AnimationColorMixer(c));
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (fill)
            {
                (font ? SpriteBatch.Font1 : SpriteBatch.Font2).DrawDynamicTextToFill(text(), bounds, color, style);
            }
        }

        public void SetColor(System.Drawing.Color c)
        {
            color.Target(c);
        }
    }
}
