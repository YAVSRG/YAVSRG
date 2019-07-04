using Interlude.Graphics;
using Interlude.Interface;

namespace Interlude.Gameplay.NoteRendering
{
    public class PlayfieldFragment
    {
        public enum InterpolationType
        {
            None,
            Linear,
            Quadratic
        }

        public struct Keyframe
        {
            public Rect TextureSource;
            public Plane Target;
            public float Time;

            public Keyframe(Rect src, Plane tgt, float time)
            {
                TextureSource = src; Target = tgt; Time = time;
            }
        }

        public Keyframe Before, After;
        public InterpolationType Easing;

        public PlayfieldFragment(Keyframe before, Keyframe after, InterpolationType easing)
        {
            Before = before;
            After = after;
            Easing = easing;
        }

        public void Draw(Sprite texture, float offset)
        {
            if (offset > After.Time) return;
            float x = 0;
            if (Easing > InterpolationType.None)
            {
                x = (offset - Before.Time) / (After.Time - Before.Time);
            }
            if (Easing == InterpolationType.Quadratic)
            {
                x *= x;
            }
            Rect uv = Before.TextureSource.Interpolate(x,After.TextureSource);
            Plane t = Before.Target.Interpolate(x, After.Target);
            //SpriteBatch.Draw(new RenderTarget(texture, );
        }
    }
}
