using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class ImageBox : Widget
    {
        public System.Drawing.Color c = System.Drawing.Color.White;
        string sprite;

        public ImageBox(string sprite)
        {
            this.sprite = sprite;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.Draw(sprite, left, top, right, bottom, c);
        }
    }
}
