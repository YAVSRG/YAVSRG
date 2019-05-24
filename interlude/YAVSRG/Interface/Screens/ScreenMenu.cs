using System.Drawing;
using Interlude.Interface.Widgets;
using Interlude.Interface.Animations;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Screens
{
    class ScreenMenu : Screen
    {
        string splash;
        string splashSub;
        AnimationSlider splashAnim, splashSubAnim;
        //AnimationCounter scroll;
        Widget play, options, quit;

        public ScreenMenu()
        {
            AddChild(
                play = new BannerButton("Play", () =>
                {
                    Game.Screens.AddScreen((Game.CurrentChart == null ? (Screen)(new ScreenImport()) : new ScreenLevelSelect()));
                }, 0.7f, 1)
                .Reposition(-100, 0, -200, 0.5f, -ScreenUtils.ScreenWidth, 0.5f, -100, 0.5f)
                );
            AddChild(
                options = new BannerButton("Options", () =>
                {
                    Game.Screens.AddScreen(new ScreenOptions());
                }, 0.7f, 1)
                .Reposition(-100, 0, -50, 0.5f, -ScreenUtils.ScreenWidth, 0.5f, 50, 0.5f)
                );
            AddChild(
                quit = new BannerButton("Quit", () =>
                {
                    Game.Screens.PopScreen();
                }, 0.7f, 1)
                .Reposition(-100, 0, 100, 0.5f, -ScreenUtils.ScreenWidth, 0.5f, 200, 0.5f)
                );
            AddChild(new NewsBox());
            Animation.Add(splashAnim = new AnimationSlider(0));
            Animation.Add(splashSubAnim = new AnimationSlider(0));
            //Animation.Add(scroll = new AnimationCounter(10000,true));
            Animation.Add(new Animation()); //this dummy animation ensures that ScreenManager handles the other animations
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            var s = ResourceGetter.MenuSplash().Split('¬');
            splash = s[0];
            splashSub = s.Length > 1 ? s[1] : "";
            if (Game.CurrentChart != null) Discord.SetPresence("Main Menu", Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title + " [" + Game.CurrentChart.Data.DiffName + "]\nFrom " + Game.CurrentChart.Data.SourcePack, true);
            Game.Screens.BackgroundDim.Target = 1;
            play.RightAnchor.Reposition(-ScreenUtils.ScreenWidth);
            options.RightAnchor.Reposition(-ScreenUtils.ScreenWidth);
            quit.RightAnchor.Reposition(-ScreenUtils.ScreenWidth);
            OnResize();
            //this won't run in OnResize if we're transitioning to this screen
            Game.Screens.Logo.Move(new Rect(-ScreenUtils.ScreenWidth, -400, -ScreenUtils.ScreenWidth + 800, 400));
        }

        public override void OnResize()
        {
            base.OnResize();
            var a = new AnimationSeries(false);
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() =>
            {
                play.RightAnchor.Move(-ScreenUtils.ScreenWidth + 1200);
            }));
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() =>
            {
                options.RightAnchor.Move(-ScreenUtils.ScreenWidth + 1130);
            }));
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() =>
            {
                quit.RightAnchor.Move(-ScreenUtils.ScreenWidth + 1060);
            }));
            a.Add(new AnimationCounter(20, false));
            a.Add(new AnimationAction(() =>
            {
                splashAnim.Target = 1;
            }));
            Animation.Add(a);
            if (Game.Screens.Current is ScreenMenu) //prevents logo moving mid transition
            Game.Screens.Logo.Move(new Rect(-ScreenUtils.ScreenWidth, -400, -ScreenUtils.ScreenWidth + 800, 400));
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            if (!(next is ScreenLoading))
            {
                Game.Screens.Logo.Move(new Rect(-ScreenUtils.ScreenWidth - 400, -200, -ScreenUtils.ScreenWidth, 200));
            }
            Game.Screens.BackgroundDim.Target = 0.3f;
            splashAnim.Target = 0;
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            splashSubAnim.Target = ScreenUtils.MouseOver(bounds.ExpandX(-400).SliceTop(100)) ? 1 : 0;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            SpriteBatch.Font1.DrawCentredText(splashSub, 20f, bounds.CenterX, bounds.Top + 50 + 30 * splashSubAnim, Color.FromArgb((int)(splashSubAnim * splashAnim * 255), Game.Options.Theme.MenuFont), true, Game.Screens.DarkColor);
            SpriteBatch.Font1.DrawCentredText(splash, 40f, bounds.CenterX, bounds.Top - 60 + 80 * splashAnim, Color.FromArgb((int)(splashAnim * 255), Game.Options.Theme.MenuFont), true, Game.Screens.DarkColor);
        }
    }
}
