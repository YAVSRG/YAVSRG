using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Graphics;
using Interlude.Options;
using Interlude.Gameplay;

namespace Interlude.Interface.Widgets.Gameplay
{
    class Mascot : GameplayWidget
    {
        int frame = 0;
        public Mascot(ScoreTracker s, WidgetPosition pos) : base(s, pos)
        {
            s.OnHit += (_, __, ___) => { frame++; };
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            Sprite s = Game.Options.Themes.GetTexture("mascot");
            float w = bounds.Height * (s.Width / s.UV_X) / s.Height;
            SpriteBatch.Draw(new RenderTarget(s, bounds.SliceRight(w), System.Drawing.Color.White, frame % s.UV_X, 0));
        }
    }
}
