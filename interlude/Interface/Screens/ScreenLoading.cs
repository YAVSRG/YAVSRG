using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static YAVSRG.Interface.ScreenUtils;
using YAVSRG.Charts;
using System.Drawing;
using YAVSRG.Interface.Animations;
using YAVSRG.Interface.Widgets;

namespace YAVSRG.Interface.Screens
{
    class ScreenLoading : Screen
    {
        static readonly string[] splashes = new[] { "Welcome to Interlude", "Give it a moment...", "Loading some stuff" };
        string splash = splashes[new Random().Next(0, splashes.Length)];
        Sprite desktop;
        AnimationFade fade;
        AnimationSeries transition;
        AnimationCounter counter;
        bool exiting;
        OpenTK.WindowBorder wb; //remembers if it needs to reenable the window border

        public ScreenLoading(Sprite s)
        {
            desktop = s;
            Animation.Add(transition = new AnimationSeries(true));
            Animation.Add(counter = new AnimationCounter(1000000000, true));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
                wb = Game.Instance.WindowBorder;
            Game.Instance.WindowBorder = OpenTK.WindowBorder.Hidden;
            Game.Screens.Logo.A.Target(-300, -300);
            Game.Screens.Logo.B.Target(300, 300);
            if (!Game.Screens.Loading)
            {
                exiting = true;
                Game.Screens.Toolbar.SetHidden(true);
                transition.Add(new AnimationCounter(100, false));
                transition.Add(fade = new AnimationFade(0, 1, 0.996f));
            }
            else
            {
                ChartLoader.Init();
                Game.Screens.Toolbar.SetHidden(false);
                transition.Add(fade = new AnimationFade(0, 1, 0.996f));
                transition.Add(new AnimationCounter(100, false));
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            var screen = OpenTK.DisplayDevice.Default;
            Rectangle bounds = new Rectangle(Game.Instance.Bounds.X, Game.Instance.Bounds.Y, Game.Instance.ClientRectangle.Width, Game.Instance.ClientRectangle.Height);
            RectangleF UV = new RectangleF((float)bounds.X/screen.Width,(float)bounds.Y/screen.Height,(float)bounds.Width/screen.Width,(float)bounds.Height/screen.Height);
            SpriteBatch.Draw(sprite:desktop, left:-ScreenWidth, top:-ScreenHeight, right:ScreenWidth, bottom:ScreenHeight, texcoords:SpriteBatch.VecArray(UV), color:Color.White);
            int a = (int)(255 * fade);
            if (exiting) { a = 255 - a; }
            else
            {
                float o = -15f * splash.Length;
                for (int i = 0; i < splash.Length; i++)
                {
                    SpriteBatch.Font1.DrawCentredText(splash[i].ToString(), 50f, o + 30 * i, -400 + 50f * (float)Math.Sin(counter.value * 0.01f + i*0.2f), Color.FromArgb(a, Color.White));
                }
            }
            DrawLoadingAnimation(350f * (exiting ? 1 - fade : fade), 0, 0, counter.value * 0.01f);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            int a = (int)(255 * fade);
            if (exiting)
            {
                a = 255 - a;
                Game.Audio.SetVolume(Game.Options.General.AudioVolume * (1 - fade));
                if (!transition.Running)
                {
                    Game.Instance.Exit();
                }
            }
            else if (ChartLoader.Loaded && !transition.Running)
            {
                Game.Screens.Loading = false;
                ChartLoader.Refresh();
                ChartLoader.RandomChart();
                Game.Screens.AddScreen(new ScreenMenu());
                Game.Instance.WindowBorder = wb;
            }
            ((Logo)Game.Screens.Logo).c = Color.FromArgb(a, Color.White);
        }
    }
}
