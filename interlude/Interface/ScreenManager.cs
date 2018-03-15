using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface
{
    class ScreenManager
    {
        AnimationFade fade1;
        AnimationFade fade2;
        AnimationSlider parallax = new AnimationSlider(15);
        public Toolbar toolbar;
        List<Screen> stack = new List<Screen>() {};
        List<Dialog> dialogs = new List<Dialog>();
        AnimationSeries animation = new AnimationSeries(true);
        AnimationGroup animation2 = new AnimationGroup(true);
        Screen Previous = null;
        public ColorFade BackgroundDim = new ColorFade(Color.FromArgb(80, 80, 80), Color.White);
        public Screen Current = null;
        public bool Loading = true;
        public Sprite Background;
        public Sprite Oldbackground;
        public AnimationColorMixer BaseColor;
        public AnimationColorMixer DarkColor;
        public AnimationColorMixer HighlightColor;

        public ScreenManager()
        {
            animation2.Add(parallax);
            animation2.Add(BackgroundDim);
            animation2.Add(BaseColor = new AnimationColorMixer(Color.White));
            animation2.Add(DarkColor = new AnimationColorMixer(Color.White));
            animation2.Add(HighlightColor = new AnimationColorMixer(Color.White));
        }

        public void AddDialog(Dialog d)
        {
            dialogs.Insert(0, d);
        }

        public void ChangeBackground(Sprite bg)
        {
            Content.UnloadTexture(Oldbackground);
            Oldbackground = Background;
            Background = bg;
        }

        public void AddScreen(Screen s)
        {
            stack.Insert(0, s);
            SetNextScreen(stack[0]);
            animation.Clear();
            fade1 = new AnimationFade(0, 2, 0.996f);
            fade2 = new AnimationFade(0, 1, 0.996f);
            animation.Add(fade1);
            animation.Add(fade2);
        }

        public void PopScreen()
        {
            stack.RemoveAt(0);
            SetNextScreen(stack[0]);
            animation.Clear();
            fade1 = new AnimationFade(0, 2, 0.996f);
            fade2 = new AnimationFade(0, 1, 0.996f);
            animation.Add(fade1);
            animation.Add(fade2);
        }

        private void SetNextScreen(Screen s)
        {
            s?.OnEnter(Current);
            Current?.OnExit(s);
            Previous = Current;
            Current = s;
        }

        public void Toolbar(bool x)
        {
            if (x) { toolbar.Expand(); } else { toolbar.Collapse(); };
        }

        public void Draw()
        {
            if (Loading)
            {
                Current?.Draw(-Width, -Height, Width, Height);
                return;
            }
            float parallaxX = Input.MouseX * parallax / Width;
            float parallaxY = Input.MouseY * parallax / Height;
            DrawChartBackground(-Width - parallaxX - parallax, -Height - parallaxY - parallax, Width - parallaxX + parallax, Height - parallaxY + parallax, BackgroundDim);
            if (animation.Running)
            {
                if (fade1.Running)
                {
                    Previous?.Draw(-Width, -Height, Width, Height);
                    DrawChartBackground(-Width - parallaxX - parallax, -Height - parallaxY - parallax, Width - parallaxX + parallax, Height - parallaxY + parallax, Color.FromArgb((int)(255 * fade1), BackgroundDim));
                }
                else
                {
                    Current?.Draw(-Width, -Height, Width, Height);
                    DrawChartBackground(-Width - parallaxX - parallax, -Height - parallaxY - parallax, Width - parallaxX + parallax, Height - parallaxY + parallax, Color.FromArgb((int)(255 * (1-fade2)), BackgroundDim));
                }
            }
            else
            {
                Current?.Draw(-Width, -Height, Width, Height);
            }
            if (dialogs.Count > 0)
            {
                dialogs[0].Draw(-Width, -Height, Width, Height);
            }
            toolbar.Draw(-Width, -Height, Width, Height);
        }

        public void DrawChartBackground(float left, float top, float right, float bottom, Color c)
        {
            float bg = ((float)Background.Width / Background.Height);
            float window = (right - left) / (bottom - top);
            float correction = window / bg;
            RectangleF uv = new RectangleF(0, (correction - 1) * 0.5f, 1, 2 - correction);
            SpriteBatch.Draw(Background, left, top, right, bottom + 1, uv, c);
        }

        public void DrawStaticChartBackground(float left, float top, float right, float bottom, Color c)
        {
            float bg = ((float)Background.Width / Background.Height);
            float window = ((float)Width / Height);
            float correction = window / bg;

            float l = (1 + left / Width) / 2;
            float r = (1 + right / Width) / 2;
            float t = (correction + top / Height) / (2 * correction);
            float b = (correction + bottom / Height) / (2 * correction);

            RectangleF uv = new RectangleF(l, t, r - l, b - t);
            SpriteBatch.Draw(Background, left, top, right, bottom + 1, uv, c);
        }

        public void Update()
        {
            if (Loading)
            {
                Current?.Update(-Width, -Height, Width, Height);
                return;
            }
            if (dialogs.Count > 0)
            {
                dialogs[0].Update(-Width, -Height, Width, Height);
                if (dialogs[0].Closed)
                {
                    dialogs.RemoveAt(0);
                }
            }
            else
            {
                Current?.Update(-Width, -Height, Width, Height);
            }
            if (Previous != null)
            {
                if (animation.Running)
                {
                    if (Previous.Animation.Running)
                    {
                        Previous.Animation.Update();
                    }
                }
                else
                {
                    Previous = null;
                }
            }
            toolbar.Update(-Width, -Height, Width, Height);
            animation.Update();
            animation2.Update();
        }
    }
}
