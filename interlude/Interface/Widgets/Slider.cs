using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Interface.Widgets
{
    public class Slider : Widget
    {
        private Func<float> get;
        private Action<float> set;
        float max;
        float min;
        float resolution;
        string label;

        public Slider(string label, Action<float> set, Func<float> get, float min, float max, float res) : base()
        {
            this.label = label;
            this.get = get;
            this.set = set;
            this.max = max;
            this.min = min;
            resolution = res;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top + 20, right, bottom - 20, System.Drawing.Color.Gray);
            float p = left + (get() - min) / (max - min) * (right-left);
            SpriteBatch.DrawRect(p - 5, top, p + 5, bottom, System.Drawing.Color.White);
            SpriteBatch.Font2.DrawCentredText(label+": "+get().ToString(), 20f, (left+right)/2, top-30, Game.Options.Theme.MenuFont);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            if (ScreenUtils.MouseOver(left, top, right, bottom))
            {
                if (Input.KeyTap(OpenTK.Input.Key.Left) && get() - resolution > min)
                {
                    set(get() - resolution);
                }
                if (Input.KeyTap(OpenTK.Input.Key.Right) && get() + resolution < max)
                {
                    set(get() + resolution);
                }
                if (Input.MousePress(OpenTK.Input.MouseButton.Left))
                {
                    set(min + (Input.MouseX - left) / (right - left) * (max - min));
                    set(get() - get() % resolution);
                }
            }
        }
    }
}
