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
        bool dragging;

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
            SpriteBatch.DrawRect(bounds.ExpandY(-20), Game.Screens.DarkColor);
            float p = bounds.Left + (get() - min) / (max - min) * bounds.Width;
            SpriteBatch.DrawRect(new Rect(p - bounds.Height / 2, bounds.Top, p + bounds.Height / 2, bounds.Bottom), Game.Screens.HighlightColor);
            SpriteBatch.Font2.DrawCentredText(label + ": " + get().ToString(), 20f, bounds.CenterX, bounds.Top - 25, Game.Options.Theme.MenuFont);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.CheckButtonClick(bounds))
            {
                dragging = true;
            }
            else if (!Input.MousePress(OpenTK.Input.MouseButton.Left))
            {
                dragging = false;
            }

            if (dragging)
            {
                SetWithRounding(min + (Input.MouseX - bounds.Left) / bounds.Width * (max - min));
            }
            else if (ScreenUtils.MouseOver(bounds))
            {
                if (Game.Options.General.Hotkeys.Previous.Tapped())
                {
                    SetWithRounding(get() - resolution);
                }
                else if (Game.Options.General.Hotkeys.Next.Tapped())
                {
                    SetWithRounding(get() + resolution);
                }
            }
        }

        void SetWithRounding(float value)
        {
            value = (float)Math.Round(value / resolution, 0) * resolution;
            value = Math.Min(max, value);
            value = Math.Max(min, value);
            set(value);
        }
    }
}
