using Interlude.Graphics;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    //todo: maybe deprecate since unused
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
            SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture(sprite), bounds, c));
        }
    }
}
