using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using static YAVSRG.Interface.ScreenUtils;
using System.Drawing;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Screens
{
    class ScreenLoading : Screen
    {
        Sprite desktop, logo;
        AnimationFade fade;
        AnimationSeries transition;
        bool exiting;
        OpenTK.WindowBorder wb;

        public ScreenLoading(Sprite s)
        {
            desktop = s;
            logo = Content.LoadTextureFromAssets("logo");
            Animation.Add(transition = new AnimationSeries(true));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
                wb = Game.Instance.WindowBorder;
            Game.Instance.WindowBorder = OpenTK.WindowBorder.Hidden;
            if (!Game.Screens.Loading)
            {
                exiting = true;
                Game.Screens.toolbar.SetHidden(true);
                transition.Add(new AnimationCounter(100, false));
                transition.Add(fade = new AnimationFade(0, 1, 0.996f));
            }
            else
            {
                Game.Screens.toolbar.SetHidden(false);
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
            SpriteBatch.Draw(desktop, -ScreenWidth, -ScreenHeight, ScreenWidth, ScreenHeight, SpriteBatch.VecArray(UV), Color.White);
            int a = (int)(255 * fade);
            if (exiting) { a = 255 - a; }
            SpriteBatch.Draw(logo, -250, -250, 250, 250, Color.FromArgb(a, Color.White));
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);

            if (!fade.Running && !ChartLoader.Loaded)
            {
                ChartLoader.Init();
                ChartLoader.RandomChart();
            }
            else if (exiting)
            {
                Game.Audio.SetVolume(Game.Options.General.AudioVolume * (1 - fade));
                if (!transition.Running)
                {
                    Game.Instance.Exit();
                }
            }
            else if (!transition.Running)
            {
                Game.Screens.Loading = false;
                Game.Screens.AddScreen(new ScreenMenu());
                Game.Instance.WindowBorder = wb;
            }
        }
    }
}
