using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Interface.Animations;
using OpenTK;

namespace YAVSRG.Interface
{
    class ScreenManager
    {
        AnimationFade fade1; //prev screen fade out 
        AnimationFade fade2; //next screen fade in
        AnimationSlider parallax = new AnimationSlider(20);
        public Toolbar toolbar;
        List<Screen> stack = new List<Screen>() {};
        List<Dialog> dialogs = new List<Dialog>();
        AnimationSeries animation = new AnimationSeries(true);
        AnimationGroup animation2 = new AnimationGroup(true);
        Screen Previous = null;
        public ColorFade BackgroundDim = new ColorFade(Color.Black, Color.White);
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
            if (bg.ID != Oldbackground.ID)
            {
                Content.UnloadTexture(Oldbackground);
            }
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

        public void Draw()
        {
            if (Loading)
            {
                Current?.Draw(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight);
                return;
            }
            DrawChartBackground(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight, BackgroundDim);
            if (animation.Running)
            {
                if (fade1.Running)
                {
                    Previous?.Draw(-ScreenWidth, -ScreenHeight + toolbar.Height, ScreenWidth, ScreenHeight - toolbar.Height);
                    DrawChartBackground(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight, Color.FromArgb((int)(255 * fade1), BackgroundDim));
                }
                else
                {
                    Current?.Draw(-ScreenWidth, -ScreenHeight + toolbar.Height, ScreenWidth, ScreenHeight - toolbar.Height);
                    DrawChartBackground(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight, Color.FromArgb((int)(255 * (1-fade2)), BackgroundDim));
                }
            }
            else
            {
                Current?.Draw(-ScreenWidth, -ScreenHeight + toolbar.Height, ScreenWidth, ScreenHeight - toolbar.Height);
            }
            if (dialogs.Count > 0)
            {
                dialogs[0].Draw(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight);
            }
            toolbar.Draw(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight);
        }

        public void DrawChartBackground(float left, float top, float right, float bottom, Color c, float parallaxMult = 1f)
        {
            float parallaxX = parallaxMult * Input.MouseX * parallax / ScreenWidth;
            float parallaxY = parallaxMult * Input.MouseY * parallax / ScreenHeight;

            float bg = ((float)Background.Width / Background.Height);
            float window = (ScreenWidth + parallax) / (ScreenHeight + parallax);
            float correction = window / bg;

            float l = (1 + (left + parallaxX) / (ScreenWidth + parallax * 2)) / 2;
            float r = (1 + (right + parallaxX) / (ScreenWidth + parallax * 2)) / 2;
            float t = (correction + (top + parallaxY) / (ScreenHeight + parallax * 2)) / (2 * correction);
            float b = (correction + (bottom + parallaxY) / (ScreenHeight + parallax * 2)) / (2 * correction);

            Vector2[] v = new[]
            {
                new Vector2(l,t),
                new Vector2(r,t),
                new Vector2(r,b),
                new Vector2(l,b)
            };
            SpriteBatch.Draw(Background, left, top, right, bottom + 1, v, c);
        }

        public void Update()
        {
            toolbar.Update(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight);
            if (Loading)
            {
                Current?.Update(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight);
                return;
            }
            if (dialogs.Count > 0)
            {
                dialogs[0].Update(-ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight);
                if (dialogs[0].Closed)
                {
                    dialogs.RemoveAt(0);
                }
            }
            else
            {
                Current?.Update(-ScreenWidth, -ScreenHeight + toolbar.Height, ScreenWidth, ScreenHeight - toolbar.Height);
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
            animation.Update();
            animation2.Update();
        }
    }
}
