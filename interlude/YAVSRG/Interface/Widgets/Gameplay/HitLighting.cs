using System.Drawing;
using Interlude.Interface.Animations;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Gameplay
{
    class HitLighting : Widget
    {
        AnimationSlider ReceptorLight = new AnimationSlider(0);
        Bind KeyBind;
        float scale;

        public HitLighting(Bind bind) : base()
        {
            KeyBind = bind;
            scale = Game.Options.Theme.ColumnWidth / Game.Options.Themes.GetTexture("receptorlighting").Width;
            Animation.Add(ReceptorLight);
        }

        public override void Update(Rect bounds)
        {
            if (KeyBind.Held()) ReceptorLight.Val = 1;
            base.Update(bounds);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float w = bounds.Width;
            float threshold = 1 - Game.Options.Theme.ColumnLightTime;
            if (ReceptorLight.Val > threshold)
            {
                int a = (int)(255f * (ReceptorLight.Val - threshold) / Game.Options.Theme.ColumnLightTime);
                SpriteBatch.DrawAlignedTexture("receptorlighting", bounds.CenterX, bounds.Top, scale * ReceptorLight.Val, scale / ReceptorLight.Val, -0.5f, -1, Color.FromArgb(a, Color.White));
            }
        }
    }
}
