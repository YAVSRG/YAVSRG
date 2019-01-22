using YAVSRG.Graphics;

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

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Draw(sprite, bounds, c);
        }
    }
}
