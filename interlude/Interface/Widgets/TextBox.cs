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
        float size;
        Animations.AnimationColorMixer color;

        public TextBox(string text, AnchorType position, float textSize, bool altFont, System.Drawing.Color c) : this(() => { return text; }, position, textSize, altFont, c) { }

        public TextBox(Func<string> text, AnchorType position, float textSize, bool altFont, System.Drawing.Color c)
        {
            this.text = text;
            style = position;
            font = altFont;
            size = textSize;
            Animation.Add(color = new Animations.AnimationColorMixer(c));
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            if (size == 0)
            {
                (font ? SpriteBatch.Font1 : SpriteBatch.Font2).DrawDynamicTextToFill(text(), bounds, color, style);
            }
            else
            {
                (font ? SpriteBatch.Font1 : SpriteBatch.Font2).DrawDynamicText(text(), bounds, color, style, size);
            }
        }

        public void SetColor(System.Drawing.Color c)
        {
            color.Target(c);
        }
    }
}
