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

        AnimationSeries animation = new AnimationSeries(true);
        AnimationGroup animation2 = new AnimationGroup(true);

        List<Screen> stack = new List<Screen>() { };
        List<Dialog> dialogs = new List<Dialog>();
        Screen Previous = null;
        public Screen Current = null;
        public bool Loading = true;

        public Sprite Background;
        public Sprite Oldbackground; //unlikely to ever be used / would be for fading because it needs to calculate two separate aspect ratio adjustments
        public AnimationColorFade BackgroundDim = new AnimationColorFade(Color.Black, Color.White);

        public AnimationColorMixer BaseColor;
        public AnimationColorMixer DarkColor;
        public AnimationColorMixer HighlightColor;
        
        public AnimationSlider Parallax = new AnimationSlider(15);
        public Widgets.Logo Logo;
        public Toolbar Toolbar;

        private AnchorPoint ParallaxPos = new AnchorPoint(0, 0, AnchorType.MIN, AnchorType.MIN);
        private Func<Point> ParallaxFunc = () => new Point(Input.MouseX, Input.MouseY);

        public ScreenManager()
        {
            animation2.Add(Parallax);
            animation2.Add(ParallaxPos);
            animation2.Add(BackgroundDim);
            animation2.Add(BaseColor = new AnimationColorMixer(Color.White));
            animation2.Add(DarkColor = new AnimationColorMixer(Color.White));
            animation2.Add(HighlightColor = new AnimationColorMixer(Color.White));
            Logo = new Widgets.Logo();
            Logo.PositionTopLeft(-200, 1000, AnchorType.CENTER, AnchorType.CENTER).PositionBottomRight(200, 1400, AnchorType.CENTER, AnchorType.CENTER);
        }

        public void AddDialog(Dialog d)
        {
            dialogs.Insert(0, d);
        }

        public void ChangeBackground(Sprite bg)
        {
            if (!(bg.ID == Background.ID || Background.ID == Content.GetTexture("background").ID))
            {
                Content.UnloadTexture(Background);
            }
            Oldbackground = Background; //oldbackground is unused
            Background = bg;
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

        public void SetParallaxOverride(Func<Point> f) //used to move the parallax effect to some place other than the mouse position i.e in visualiser
        {
            if (f == null)
            {
                f = () => new Point(Input.MouseX, Input.MouseY);
            }
            ParallaxFunc = f;
        }

        public void Draw()
        {
            Rect bounds = Bounds;
            if (Loading)
            {
                Current?.Draw(bounds);
                Logo.Draw(bounds);
                return;
            }
            DrawChartBackground(bounds, BackgroundDim);
            if (animation.Running)
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
            if (dialogs.Count > 0)
            {
                dialogs[0].Draw(bounds.ExpandY(-Toolbar.Height));
            }
            Logo.Draw(bounds);
            Toolbar.Draw(bounds);
        }

        public void DrawChartBackground(Rect bounds, Color c, float parallaxMult = 1f)
        {
            //this draws the background of the chart on the screen
            //a section of the texture is selected such all parts of the screen line up with the overall background image being fitted to the whole screen

            float parallaxX = parallaxMult * ParallaxPos.AbsoluteX * Parallax / ScreenWidth; //this calculates parallax from mouse position
            float parallaxY = parallaxMult * ParallaxPos.AbsoluteY * Parallax / ScreenHeight;

            float bg = ((float)Background.Width / Background.Height);
            float window = (ScreenWidth + Parallax) / (ScreenHeight + Parallax);
            float correction = window / bg; //this is aspect ratio correction (otherwise image would stretch wrong if not same ratio as window)

            float l = (1 + (bounds.Left + parallaxX) / (ScreenWidth + Parallax * 2)) / 2;
            float r = (1 + (bounds.Right + parallaxX) / (ScreenWidth + Parallax * 2)) / 2;
            float t = (correction + (bounds.Top + parallaxY) / (ScreenHeight + Parallax * 2)) / (2 * correction);
            float b = (correction + (bounds.Bottom + parallaxY) / (ScreenHeight + Parallax * 2)) / (2 * correction); //this determines the texcoords to use to achieve the effect

            Vector2[] v = new[] //package texcoords into array to use them
            {
                new Vector2(l,t),
                new Vector2(r,t),
                new Vector2(r,b),
                new Vector2(l,b)
            };
            bounds.Bottom += 1; //fix for rounding issues causing bgs to be 1 pixel too short on the screen
            SpriteBatch.Draw(sprite: Background, bounds: bounds, texcoords: v, color: c);
        }

        public void Update()
        {
            Rect bounds = Bounds;
            if (dialogs.Count == 0) Toolbar.Update(bounds);
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
                    dialogs.RemoveAt(0);
                }
            }
            else
            {
                Current?.Update(bounds.ExpandY(-Toolbar.Height));
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
            var p = ParallaxFunc();
            ParallaxPos.Target(p.X, p.Y);
            if (Input.KeyTap(Game.Options.General.Binds.CollapseToToolbar))
            {
                Game.Instance.CollapseToIcon();
            }
        }
    }
}
