using System;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
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

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds.ExpandY(-20), System.Drawing.Color.Gray);
            float p = bounds.Left + (get() - min) / (max - min) * bounds.Width;
            SpriteBatch.DrawRect(new Rect(p - 5, bounds.Top, p + 5, bounds.Bottom), System.Drawing.Color.White);
            SpriteBatch.Font2.DrawCentredText(label + ": " + get().ToString(), 20f, bounds.CenterX, bounds.Top, Game.Options.Theme.MenuFont);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.MouseOver(bounds.ExpandX(2)))
            {
                if (Input.KeyTap(OpenTK.Input.Key.Left) && get() - resolution >= min)
                {
                    set((float)Math.Round(get() - resolution, 2));
                }
                if (Input.KeyTap(OpenTK.Input.Key.Right) && get() + resolution <= max)
                {
                    set((float)Math.Round(get() + resolution, 2));
                }
                if (Input.MousePress(OpenTK.Input.MouseButton.Left))
                {
                    set(min + (Input.MouseX - bounds.Left) / bounds.Width * (max - min));
                    set((float)Math.Round(get() - get() % resolution, 2));
                }
            }
        }
    }
}
