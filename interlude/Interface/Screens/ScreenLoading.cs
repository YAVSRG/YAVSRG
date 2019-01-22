using System;
using System.Drawing;
using YAVSRG.Gameplay;
using YAVSRG.Interface.Animations;
using YAVSRG.Graphics;
using static YAVSRG.Interface.ScreenUtils;

namespace YAVSRG.Interface.Screens
{
    class ScreenLoading : Screen
    {
        string splash = IO.ResourceGetter.LoadingSplash();
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
            wb = Game.Options.General.WindowMode == Options.General.WindowType.Borderless ? OpenTK.WindowBorder.Hidden : OpenTK.WindowBorder.Resizable;
            Game.Screens.Toolbar.SetState(WidgetState.DISABLED);
            if (Game.Instance.WindowBorder != OpenTK.WindowBorder.Hidden)
            {
                Game.Instance.WindowBorder = OpenTK.WindowBorder.Hidden;
            }
            if (!Game.Screens.Loading)
            {
                Game.Screens.Logo.Move(new Rect(-300, -300, 300, 300), false);
                Game.Screens.Toolbar.SetCursorState(false);
                exiting = true;
                transition.Add(new AnimationCounter(100, false));
                transition.Add(fade = new AnimationFade(0, 1, 0.999f));
            }
            else
            {
                ChartLoader.Init();
                transition.Add(new AnimationCounter(10, false));
                transition.Add(new AnimationAction(() =>
                {
                    Game.Audio.PlaySFX("hello");
                    Game.Screens.Logo.Move(new Rect(-300, -300, 300, 300), false);
                }));
                transition.Add(fade = new AnimationFade(0, 1, 0.996f));
                transition.Add(new AnimationCounter(100, false));
            }
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            var screen = OpenTK.DisplayDevice.Default;
            Rectangle screenBounds = new Rectangle(Game.Instance.Bounds.X, Game.Instance.Bounds.Y, Game.Instance.ClientRectangle.Width, Game.Instance.ClientRectangle.Height);
            RectangleF UV = new RectangleF((float)screenBounds.X / screen.Width, (float)screenBounds.Y / screen.Height, (float)screenBounds.Width / screen.Width, (float)screenBounds.Height / screen.Height);
            SpriteBatch.Draw(sprite: desktop, bounds: Bounds, texcoords: SpriteBatch.VecArray(UV), color: Color.White);
            int a = (int)(255 * fade);
            if (exiting) { a = 255 - a; }
            else
            {
                float o = -15f * splash.Length;
                for (int i = 0; i < splash.Length; i++)
                {
                    SpriteBatch.Font1.DrawCentredText(splash[i].ToString(), 50f, o + 30 * i, -400 + 50f * (float)Math.Sin(counter.value * 0.01f + i * 0.2f), Color.FromArgb(a, Color.White));
                }
            }
            DrawLoadingAnimation(350f * (exiting ? 1 - fade : fade), 0, 0, counter.value * 0.01f, a);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
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
            else if (!transition.Running)
            {
                Game.Screens.Loading = false;
                ChartLoader.Refresh();
                ChartLoader.RandomChart();
                Game.Screens.AddScreen(new ScreenMenu());
                Game.Screens.Toolbar.SetState(WidgetState.ACTIVE);
                Game.Instance.WindowBorder = wb;
            }
            Game.Screens.Logo.alpha = a / 255f;
        }
    }
}
