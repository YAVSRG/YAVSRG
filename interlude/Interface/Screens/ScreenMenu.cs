using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface;
using YAVSRG.Interface.Widgets;
using YAVSRG.Interface.Animations;
using System.Drawing;

namespace YAVSRG.Interface.Screens
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
                    Game.Screens.AddScreen(new ScreenLevelSelect());
                })
                .PositionTopLeft(-100, -200, AnchorType.MIN, AnchorType.CENTER)
                .PositionBottomRight(-ScreenUtils.ScreenWidth, -100, AnchorType.CENTER, AnchorType.CENTER)
                );
            AddChild(
                options = new BannerButton("Options", () =>
                {
                    Game.Screens.AddScreen(new ScreenOptions());
                })
                .PositionTopLeft(-100, -50, AnchorType.MIN, AnchorType.CENTER)
                .PositionBottomRight(-ScreenUtils.ScreenWidth, 50, AnchorType.CENTER, AnchorType.CENTER)
                );
            AddChild(
                quit = new BannerButton("Quit", () =>
                {
                    Game.Screens.PopScreen();
                })
                .PositionTopLeft(-100, 100, AnchorType.MIN, AnchorType.CENTER)
                .PositionBottomRight(-ScreenUtils.ScreenWidth, 200, AnchorType.CENTER, AnchorType.CENTER)
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
            var s = Utilities.ResourceGetter.MenuSplash().Split('¬');
            splash = s[0];
            splashSub = s.Length > 1 ? s[1] : "";
            Utilities.Discord.SetPresence("Main Menu", Game.CurrentChart.Data.Artist + " - " + Game.CurrentChart.Data.Title + " [" + Game.CurrentChart.Data.DiffName + "]\nFrom " + Game.CurrentChart.Data.SourcePack, true);
            Game.Screens.BackgroundDim.Target = 1;
            play.PositionBottomRight(-ScreenUtils.ScreenWidth, -100, AnchorType.CENTER, AnchorType.CENTER);
            options.PositionBottomRight(-ScreenUtils.ScreenWidth, 50, AnchorType.CENTER, AnchorType.CENTER);
            quit.PositionBottomRight(-ScreenUtils.ScreenWidth, 200, AnchorType.CENTER, AnchorType.CENTER);
            OnResize();
            //this won't run in OnResize since we're transitioning to this screen
            Game.Screens.Logo.Move(new Rect(-ScreenUtils.ScreenWidth, -400, -ScreenUtils.ScreenWidth + 800, 400), false);
        }

        public override void OnResize()
        {
            base.OnResize();
            var a = new AnimationSeries(false);
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() =>
            {
                play.RightAnchor.Move(-ScreenUtils.ScreenWidth + 1200, false);
            }));
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() =>
            {
                options.RightAnchor.Move(-ScreenUtils.ScreenWidth + 1130, false);
            }));
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() =>
            {
                quit.RightAnchor.Move(-ScreenUtils.ScreenWidth + 1060, false);
            }));
            a.Add(new AnimationCounter(20, false));
            a.Add(new AnimationAction(() =>
            {
                splashAnim.Target = 1;
            }));
            Animation.Add(a);
            if (Game.Screens.Current is ScreenMenu) //prevents logo moving mid transition
            Game.Screens.Logo.Move(new Rect(-ScreenUtils.ScreenWidth, -400, -ScreenUtils.ScreenWidth + 800, 400), false);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            if (!(next is ScreenLoading))
            {
                Game.Screens.Logo.Move(new Rect(-ScreenUtils.ScreenWidth - 400, -200, -ScreenUtils.ScreenWidth, 200), false);
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
            //float w = SpriteBatch.Font1.MeasureText(splash, 30f) / 2 + 10;
            //SpriteBatch.DrawRect(
            //    new Rect(bounds.CenterX - w + (ScreenUtils.ScreenWidth) * (1 - slide), bounds.Top + 30, bounds.CenterX + w + (ScreenUtils.ScreenWidth) * (1 - slide), bounds.Top + 100),
            //    Color.FromArgb(100,Game.Screens.DarkColor));
            SpriteBatch.Font1.DrawCentredText(splashSub, 20f, bounds.CenterX, bounds.Top + 50 + 40 * splashSubAnim, Color.FromArgb((int)(splashSubAnim * splashAnim * 255), Game.Options.Theme.MenuFont), true, Game.Screens.DarkColor);
            SpriteBatch.Font1.DrawCentredText(splash, 40f, bounds.CenterX, bounds.Top - 60 + 80 * splashAnim, Color.FromArgb((int)(splashAnim * 255), Game.Options.Theme.MenuFont), true, Game.Screens.DarkColor);
        }
    }
}
