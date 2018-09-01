using System;
using System.Collections.Generic;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Widgets.Gameplay
{
    class HitLighting : Widget
    {
        public AnimationSlider NoteLight = new AnimationSlider(0);
        public AnimationSlider ReceptorLight = new AnimationSlider(0);

        public HitLighting() : base()
        {
            Animation.Add(NoteLight);
            Animation.Add(ReceptorLight);
        }

        public override void Draw(Rect bounds) //draws hitlight, right now just the receptor light and not a flash when you hit a note
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            float w = bounds.Width;
            if (ReceptorLight.Val > 0.5f)
            {
                SpriteBatch.Draw("receptorlighting", new Rect(bounds.Left + w * (1 - ReceptorLight.Val), bounds.Top + 3 * w * (1 - ReceptorLight.Val), bounds.Right - w * (1 - ReceptorLight.Val), bounds.Bottom), Color.White);
            }
            if (NoteLight.Val > 0f)
            {
                //slice
                SpriteBatch.Draw("notelighting", new Rect(bounds.Left, bounds.Bottom + w, bounds.Right, bounds.Bottom), Color.FromArgb((int)(NoteLight.Val * 255), Color.White));
            }
        }
    }
}
