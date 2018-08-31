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

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (fill)
            {
                (font ? SpriteBatch.Font1 : SpriteBatch.Font2).DrawDynamicTextToFill(text(), left, top, right, bottom, color, style);
            }
        }

        public void SetColor(System.Drawing.Color c)
        {
            color.Target(c);
        }
    }
}
