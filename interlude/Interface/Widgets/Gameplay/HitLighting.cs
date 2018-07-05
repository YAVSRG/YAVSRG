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

        public override void Draw(float left, float top, float right, float bottom) //draws hitlight, right now just the receptor light and not a flash when you hit a note
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            float w = (right - left);
            if (ReceptorLight.Val > 0.5f)
            {
                SpriteBatch.Draw("receptorlighting", left + w * (1 - ReceptorLight.Val), top + 3 * w * (1 - ReceptorLight.Val), right - w * (1 - ReceptorLight.Val), bottom, Color.White);
            }
            if (NoteLight.Val > 0f)
            {
                SpriteBatch.Draw("notelighting", left, bottom + w, right, bottom, Color.FromArgb((int)(NoteLight.Val * 255), Color.White));
            }
        }
    }
}
