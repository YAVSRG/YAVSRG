using System.Drawing;
using Interlude.Interface.Animations;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets.Gameplay
{
    class HitLighting : Widget
    {
        AnimationSlider NoteLight = new AnimationSlider(0);
        AnimationSlider ReceptorLight = new AnimationSlider(0);
        Bind KeyBind;
        float scale;

        public HitLighting(Bind bind) : base()
        {
            KeyBind = bind;
            scale = Game.Options.Theme.ColumnWidth / Game.Options.Themes.GetTexture("receptorlighting").Width;
            Animation.Add(NoteLight);
            Animation.Add(ReceptorLight);
        }

        public override void Update(Rect bounds)
        {
            ReceptorLight.Target = KeyBind.Held() ? 1 : 0;
            base.Update(bounds);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float w = bounds.Width;
            if (ReceptorLight.Val > Game.Options.Theme.ColumnLightTime)
            {
                int a = (int)(255 * (ReceptorLight.Val - Game.Options.Theme.ColumnLightTime) / Game.Options.Theme.ColumnLightTime);
                SpriteBatch.DrawAlignedTexture("receptorlighting", bounds.CenterX, bounds.Top, scale * ReceptorLight.Val, scale / ReceptorLight.Val, -0.5f, 0, Color.FromArgb(a, Color.White));
            }
            if (NoteLight.Val > 0f)
            {
                SpriteBatch.Draw(new RenderTarget(Game.Options.Themes.GetTexture("notelighting"), bounds.SliceBottom(w), Color.FromArgb((int)(NoteLight.Val * 255), Color.White)));
            }
        }
    }
}
