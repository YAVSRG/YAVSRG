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
        static readonly string[] splashes = new[] { "Yet Another Vertically Scrolling Rhythm Game", "Some funny flavourtext", "Based on the hit game, osu!mania",
            "Pausers never win", "Winners never pause", "Timing is everything", "Where's the pause button?", "Just play BMS", "Skill not included",
            "Click play already", "JUST MASH", "A cool name for a rhythm game", "Making rhythm games great again", "The future is now, old man",
            "https://en.wikipedia.org/wiki/Rhythm_game", "https://github.com/percyqaz/YAVSRG/issues/9", "Attention to detail" };
        string splash = splashes[new Random().Next(0, splashes.Length)];
        AnimationSlider slide;
        Sprite banner;
        Widget play, options, quit;

        public ScreenMenu()
        {
            AddChild(
                play = new BannerButton("Play", () => { Game.Screens.AddScreen(new ScreenLevelSelect()); })
                .PositionTopLeft(-100, -100, AnchorType.MIN, AnchorType.CENTER)
                .PositionBottomRight(-ScreenUtils.ScreenWidth, 0, AnchorType.CENTER, AnchorType.CENTER)
                );
            AddChild(
                options = new BannerButton("Options", () => { Game.Screens.AddScreen(new ScreenOptions()); })
                .PositionTopLeft(-100, 50, AnchorType.MIN, AnchorType.CENTER)
                .PositionBottomRight(-ScreenUtils.ScreenWidth, 150, AnchorType.CENTER, AnchorType.CENTER)
                );
            AddChild(
                quit = new BannerButton("Quit", () =>
                {
                    Game.Screens.PopScreen();
                })
                .PositionTopLeft(-100, 200, AnchorType.MIN, AnchorType.CENTER)
                .PositionBottomRight(-ScreenUtils.ScreenWidth, 300, AnchorType.CENTER, AnchorType.CENTER)
                );
            slide = new AnimationSlider(0);
            banner = Content.LoadTextureFromAssets("banner");
            slide.Target = 1;
            Animation.Add(slide);
            Animation.Add(new Animation()); //this dummy animation ensures that ScreenManager handles the other animations
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            splash = splashes[new Random().Next(0, splashes.Length)];
            Game.Screens.BackgroundDim.Target = 1;
            slide.Target = 1;
            play.B.Reposition(-ScreenUtils.ScreenWidth, 0, AnchorType.CENTER, AnchorType.CENTER);
            options.B.Reposition(-ScreenUtils.ScreenWidth, 150, AnchorType.CENTER, AnchorType.CENTER);
            quit.B.Reposition(-ScreenUtils.ScreenWidth, 300, AnchorType.CENTER, AnchorType.CENTER);
            var a = new AnimationSeries(false);
            a.Add(new AnimationCounter(10,false));
            a.Add(new AnimationAction(() => {
                play.B.Target(0, 0);
            }));
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() => {
                options.B.Target(-70, 150);
            }));
            a.Add(new AnimationCounter(10, false));
            a.Add(new AnimationAction(() => {
                quit.B.Target(-140, 300);
            }));
            Animation.Add(a);
        }

        public override void OnExit(Screen next)
        {
            base.OnExit(next);
            Game.Screens.BackgroundDim.Target = 0;
            slide.Target = 0;
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            float w = (right-100) * slide;
            ScreenUtils.DrawBanner(banner, -w, -300, w, -200, Game.Screens.HighlightColor);
            SpriteBatch.Font1.DrawCentredText("Interlude", 50f, 0, -300, Game.Screens.HighlightColor);
            SpriteBatch.Font2.DrawCentredText(splash, 20f, 0, -240, Game.Screens.HighlightColor);
        }
    }
}
