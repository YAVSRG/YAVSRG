using System;
using System.Collections.Generic;
using System.Drawing;
using Interlude.Interface.Animations;
using Interlude.IO;
using Interlude.Graphics;
using static Interlude.Interface.ScreenUtils;

namespace Interlude.Interface
{
    class ScreenManager
    {
        AnimationFade fade1; //prev screen fade out 
        AnimationFade fade2; //next screen fade in

        AnimationSlider bgFade; //fade between previous background and current

        AnimationSeries screenAnimation = new AnimationSeries(true); //coordinates fading between screens
        AnimationGroup animation = new AnimationGroup(true); //coordinates all other animations (they run constantly)

        FBO FBO; //fbo stores texture for fade between background images (it is reused many times per frame so this saves on aspect ratio calculations etc)

        protected List<Screen> stack = new List<Screen>() { }; //stack of screens - clicking back pops one off and goes to the previous screen
        protected List<Dialog> dialogs = new List<Dialog>(); //stack of dialogs - these prompt the user for something and block interaction with the screen
        protected Screen Previous = null;
        //todo: make this protected and remove references
        public Screen Current = null;
        //todo: fix this being public and controlled by ScreenLoading
        public bool Loading = true; //disables some aspects of interface while showing enter/exit animation

        public Sprite Background;
        public Sprite Oldbackground;
        public AnimationColorFade BackgroundDim = new AnimationColorFade(Color.Black, Color.White);

        //todo: move to some kind of theme class
        public AnimationColorMixer BaseColor;
        public AnimationColorMixer DarkColor;
        public AnimationColorMixer HighlightColor;
        
        public AnimationSlider Parallax = new AnimationSlider(15);
        public Widgets.Logo Logo;
        public Toolbar Toolbar;

        private readonly AnimationSlider ParallaxPosX = new AnimationSlider(0);
        private readonly AnimationSlider ParallaxPosY = new AnimationSlider(0);
        private Func<Point> ParallaxFunc = () => new Point(Input.MouseX, Input.MouseY);

        AnimationSlider _TooltipFade, _TooltipFade2;
        string[] Tooltip, Tooltip2;
        float TooltipWidth;

        public ScreenManager()
        {
            animation.Add(Parallax);
            animation.Add(ParallaxPosX);
            animation.Add(ParallaxPosY);
            animation.Add(BackgroundDim);
            animation.Add(BaseColor = new AnimationColorMixer(Color.White));
            animation.Add(DarkColor = new AnimationColorMixer(Color.White));
            animation.Add(HighlightColor = new AnimationColorMixer(Color.White));
            animation.Add(bgFade = new AnimationSlider(1));
            Logo = new Widgets.Logo();
            Logo.Reposition(-200, 0.5f, 1000, 0.5f, 200, 0.5f, 1400, 0.5f);
            animation.Add(_TooltipFade = new AnimationSlider(0)); animation.Add(_TooltipFade2 = new AnimationSlider(0));
        }

        public void AddDialog(Dialog d)
        {
            dialogs.Insert(0, d);
        }

        public void ChangeBackground(Sprite bg)
        {
            if (!(Oldbackground.GL_Texture_ID == Background.GL_Texture_ID || Oldbackground.GL_Texture_ID == Game.Options.Themes.GetTexture("background").GL_Texture_ID))
            {
                Content.UnloadTexture(Oldbackground);
            }
            Oldbackground = Background;
            Background = bg;
            bgFade.Val = 0;
        }

        public void ChangeThemeColor(Color c)
        {
            DarkColor.Target(Utils.ColorInterp(c, Color.Black, 0.5f));
            BaseColor.Target(c);
            HighlightColor.Target(Utils.ColorInterp(c, Color.White, 0.5f));
        }

        public void AddScreen(Screen s)
        {
            stack.Insert(0, s);
            SetNextScreen(stack[0]);
            screenAnimation.Clear();
            fade1 = new AnimationFade(0, 2, 0.996f);
            fade2 = new AnimationFade(0, 1, 0.996f);
            screenAnimation.Add(fade1);
            screenAnimation.Add(fade2);
        }

        public void PopScreen()
        {
            stack.RemoveAt(0);
            SetNextScreen(stack[0]);
            screenAnimation.Clear();
            fade1 = new AnimationFade(0, 2, 0.996f);
            fade2 = new AnimationFade(0, 1, 0.996f);
            screenAnimation.Add(fade1);
            screenAnimation.Add(fade2);
        }

        private void SetNextScreen(Screen s)
        {
            Current?.OnExit(s);
            s?.OnEnter(Current);
            Previous = Current;
            Current = s;
        }

        public void Resize()
        {
            Current?.OnResize();
            Previous?.OnResize();
        }

        public void SetTooltip(string text, string extra)
        {
            if (text != "")
            {
                TooltipWidth = 0;
                Tooltip = text.Split('\n');
                foreach (string l in Tooltip)
                {
                    TooltipWidth = Math.Max(TooltipWidth, SpriteBatch.Font1.MeasureText(l, 30f));
                }
                Tooltip2 = extra.Split('\n');
                _TooltipFade.Target = 1;
            }
        }

        public void SetParallaxOverride(Func<Point> f) //used to move the parallax effect to some place other than the mouse position i.e in visualiser
        {
            if (f == null)
            {
                f = () => new Point(Input.MouseX, Input.MouseY);
            }
            ParallaxFunc = f;
        }

        void DrawScaledBG(Sprite bg, int alpha)
        {
            //use math.min for "fit inside screen with letterboxing"
            //math.max is "scale up until the screen is filled" which cuts off the sides/top if aspect ratio is different
            float scale = Math.Max((float)ScreenWidth * 2 / bg.Width, (float)ScreenHeight * 2 / bg.Height);
            Color c = Color.FromArgb(alpha, Color.White);
            SpriteBatch.DrawTilingTexture(bg, Bounds, bg.Width * scale, bg.Height * scale, 0.5f, 0.5f, c, c, c, c);
        }

        public void Draw()
        {
            Rect bounds = Bounds;
            using (FBO = FBO.FromPool())
            {
                DrawScaledBG(Oldbackground, 255);
                DrawScaledBG(Background, (int)(bgFade * 255));
                FBO.Unbind();
                //FBO = SpriteBatch.WaterTest(FBO);
                if (Loading)
                {
                    Current?.Draw(bounds);
                    Logo.Draw(bounds);
                    return;
                }
                DrawChartBackground(bounds, BackgroundDim);
                if (screenAnimation.Running)
                {
                    if (fade1.Running)
                    {
                        Previous?.Draw(bounds.ExpandY(-Toolbar.Height));
                        DrawChartBackground(bounds, Color.FromArgb((int)(255 * fade1), BackgroundDim));
                    }
                    else
                    {
                        Current?.Draw(bounds.ExpandY(-Toolbar.Height));
                        DrawChartBackground(bounds, Color.FromArgb((int)(255 * (1 - fade2)), BackgroundDim));
                    }
                }
                else
                {
                    Current?.Draw(bounds.ExpandY(-Toolbar.Height));
                }
                Logo.Draw(bounds);
                for (int i = dialogs.Count - 1; i >= 0; i--)
                {
                    dialogs[i].Draw(bounds.ExpandY(-Toolbar.Height));
                }
                Toolbar.Draw(bounds);

                //todo: bundle tooltips and notifications into a widget
                float f = _TooltipFade * _TooltipFade2;
                if (f >= 0.001f)
                {
                    float x = Math.Min(bounds.Right - 50 - TooltipWidth, Input.MouseX);
                    float y = Math.Min(bounds.Bottom - 100 - 45 * Tooltip.Length, Input.MouseY);
                    var b = new Rect(x + 50, y + 50, x + 50 + TooltipWidth, y + 53 + 45 * Tooltip.Length);
                    SpriteBatch.DrawRect(b, Color.FromArgb((int)(f * 180), 0, 0, 0));
                    for (int i = 0; i < Tooltip.Length; i++)
                    {
                        SpriteBatch.Font1.DrawText(Tooltip[i], 30f, b.Left, b.Top + i * 45, Color.FromArgb((int)(f * 255), Game.Options.Theme.MenuFont));
                    }
                }
            }
        }

        public void DrawChartBackground(Rect bounds, Color c, float parallaxMult = 1f)
        {
            //todo: find the bug and fix it
            //this draws the background of the chart on the screen
            //a section of the texture is selected such all parts of the screen line up with the overall background image being fitted to the whole screen

            float parallaxX = parallaxMult * Parallax * ParallaxPosX / ScreenWidth / 2; //this calculates parallax from mouse position
            float parallaxY = parallaxMult * Parallax * ParallaxPosY / ScreenHeight / 2;
            SpriteBatch.DrawTilingTexture(FBO, bounds, (ScreenWidth + Parallax * parallaxMult) * 2, (ScreenHeight + Parallax * parallaxMult) * 2, parallaxX / ScreenWidth + 0.5f, parallaxY / ScreenHeight + 0.5f, c, c, c, c);
        }

        public void Update()
        {
            Rect bounds = Bounds;
            if (dialogs.Count == 0) Toolbar.Update(bounds); else Toolbar.Animation.Update();
            Logo.Update(bounds);
            if (Loading)
            {
                Current?.Update(bounds);
                return;
            }
            if (dialogs.Count > 0)
            {
                dialogs[0].Update(bounds.ExpandY(-Toolbar.Height));
                if (dialogs[0].Closed)
                {
                    dialogs[0].Dispose();
                    dialogs.RemoveAt(0);
                }
                Current?.Animation.Update();
            }
            else
            {
                Current?.Update(bounds.ExpandY(-Toolbar.Height));
            }
            if (Previous != null)
            {
                if (screenAnimation.Running)
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
            screenAnimation.Update();
            _TooltipFade2.Target = Game.Options.General.Hotkeys.Help.Held() ? 1 : 0;
            animation.Update();
            _TooltipFade.Target = 0;
            var p = ParallaxFunc();
            ParallaxPosX.Target = p.X;
            ParallaxPosY.Target = p.Y;
            if (Game.Options.General.Hotkeys.BossKey.Tapped())
            {
                Game.Instance.CollapseToIcon();
            }
        }
    }
}
