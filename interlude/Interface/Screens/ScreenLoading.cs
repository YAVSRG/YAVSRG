using System;
using System.Drawing;
using OpenTK;
using Interlude.Gameplay;
using Interlude.Interface.Animations;
using Interlude.Graphics;
using static Interlude.Interface.ScreenUtils;

namespace Interlude.Interface.Screens
{
    class ScreenLoading : Screen
    {
        string splash = IO.ResourceGetter.LoadingSplash();
        Sprite desktop;
        AnimationFade fade;
        AnimationSeries transition;
        AnimationCounter counter;
        bool exiting;
        WindowBorder wb; //remembers if it needs to reenable the window border

        public ScreenLoading(Sprite s)
        {
            desktop = s;
            Animation.Add(transition = new AnimationSeries(true));
            Animation.Add(counter = new AnimationCounter(1000000000, true));
        }

        public override void OnEnter(Screen prev)
        {
            base.OnEnter(prev);
            wb = Game.Options.General.WindowMode == Options.General.WindowType.Borderless ? WindowBorder.Hidden : WindowBorder.Resizable;
            Game.Screens.Toolbar.SetState(WidgetState.DISABLED);
            if (Game.Instance.WindowBorder != WindowBorder.Hidden)
            {
                Game.Instance.WindowBorder = WindowBorder.Hidden;
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
            var screen = DisplayDevice.Default;
            float l = (float)Game.Instance.Bounds.X / screen.Width;
            float t = (float)Game.Instance.Bounds.Y / screen.Height;
            float r = l + (float)Game.Instance.ClientRectangle.Width / screen.Width;
            float b = t + (float)Game.Instance.ClientRectangle.Height / screen.Height;
            SpriteBatch.Draw(new RenderTarget(desktop, Bounds, Color.White, new Vector2(l, t), new Vector2(r, t), new Vector2(r, b), new Vector2(l, b)));
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
