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
        string splash = Utilities.Splashes.MenuSplash();
        AnimationSlider slide;
        AnimationCounter scroll;
        Widget play, options, quit;

        public ScreenMenu()
        {
            AddChild(
                play = new BannerButton("Play", () => { Game.Screens.AddScreen(new ScreenLevelSelect()); })
                .PositionTopLeft(-100, -200, AnchorType.MIN, AnchorType.CENTER)
                .PositionBottomRight(-ScreenUtils.ScreenWidth, -100, AnchorType.CENTER, AnchorType.CENTER)
                );
            AddChild(
                options = new BannerButton("Options", () => { Game.Screens.AddScreen(new ScreenOptions()); })
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
            slide = new AnimationSlider(0);
            slide.Target = 1;
            Animation.Add(slide);
            Animation.Add(scroll = new AnimationCounter(10000,true));
            Animation.Add(new Animation()); //this dummy animation ensures that ScreenManager handles the other animations
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            splash = Utilities.Splashes.MenuSplash();
            Game.Screens.Logo.Move(new Rect(-ScreenUtils.ScreenWidth, -400, -ScreenUtils.ScreenWidth + 800, 400));
            Game.Screens.BackgroundDim.Target = 1;
            play.PositionBottomRight(-ScreenUtils.ScreenWidth, -100, AnchorType.CENTER, AnchorType.CENTER);
            options.PositionBottomRight(-ScreenUtils.ScreenWidth, 50, AnchorType.CENTER, AnchorType.CENTER);
            quit.PositionBottomRight(-ScreenUtils.ScreenWidth, 200, AnchorType.CENTER, AnchorType.CENTER);
            var a = new AnimationSeries(false);
            a.Add(new AnimationCounter(10,false));
            a.Add(new AnimationAction(() => {
                play.BottomRight.Target(-ScreenUtils.ScreenWidth + 1200, -100);
            }));
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() => {
                options.BottomRight.Target(-ScreenUtils.ScreenWidth + 1130, 50);
            }));
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() =>
            {
                quit.BottomRight.Target(-ScreenUtils.ScreenWidth + 1060, 200);
            }));
            a.Add(new AnimationCounter(20, false));
            a.Add(new AnimationAction(() => {
                slide.Target = 1;
            }));
            Animation.Add(a);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            if (!(next is ScreenLoading))
            {
                Game.Screens.Logo.Move(new Rect(-ScreenUtils.ScreenWidth - 400, -200, -ScreenUtils.ScreenWidth, 200));
            }
            Game.Screens.BackgroundDim.Target = 0.3f;
            slide.Target = 0;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            float w = SpriteBatch.Font1.MeasureText(splash, 30f) / 2 + 10;
            SpriteBatch.DrawRect(
                new Rect(bounds.CenterX - w + (ScreenUtils.ScreenWidth) * (1 - slide), bounds.Top + 30, bounds.CenterX + w + (ScreenUtils.ScreenWidth) * (1 - slide), bounds.Top + 100),
                Color.FromArgb(100,Game.Screens.DarkColor));
            SpriteBatch.Font1.DrawCentredText(splash, 30f, bounds.CenterX + (ScreenUtils.ScreenWidth) * (slide - 1), bounds.Top + 40, Color.FromArgb((int)(slide * 255), Game.Options.Theme.MenuFont), true, Game.Screens.DarkColor);
        }
    }
}
