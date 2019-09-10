using System;
using System.Drawing;
using Interlude.Interface.Animations;
using Interlude.IO;
using Interlude.Graphics;

namespace Interlude.Interface.Dialogs
{
    public class FadeDialog : Dialog //not for specific purpose, just basis of certain dialogs that fade out the game to focus on something
    {
        protected AnimationSlider Fade;
        bool Closing;
        protected string Output = "";
        FBO FBO;

        public FadeDialog(Action<string> action) : base(action)
        {
            Animation.Add(Fade = new AnimationSlider(0) { Target = 1 });
        }

        public override void Draw(Rect bounds)
        {
            PreDraw(bounds);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(127, 0, 0, 0));
            base.Draw(bounds);
            PostDraw(bounds);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Closing && Fade < 0.02f)
            {
                Close(Output);
            }
            if (Game.Options.General.Hotkeys.Exit.Tapped() || (!ScreenUtils.MouseOver(GetBounds(bounds)) && Input.MouseClick(OpenTK.Input.MouseButton.Left)))
            {
                OnClosing();
            }
        }

        protected void PreDraw(Rect bounds)
        {
            FBO = FBO.FromPool();
        }

        protected void PostDraw(Rect bounds)
        {
            FBO.Unbind();
            Color c = Color.FromArgb((int)(255 * Fade), Color.White);
            SpriteBatch.Draw(new RenderTarget(FBO, ScreenUtils.Bounds, c));
            FBO.Dispose();
        }

        protected virtual void OnClosing()
        {
            //so dialogs that extend this can also play other animations when closing
            Closing = true;
            Fade.Target = 0f;
        }
    }
}
