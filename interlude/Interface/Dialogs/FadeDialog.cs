using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;

namespace YAVSRG.Interface.Dialogs
{
    public class FadeDialog : Dialog //not for specific purpose, just basis of certain dialogs that fade out the game to focus on something
    {
        protected AnimationSlider Fade;
        bool Closing;
        protected string Output = "";

        public FadeDialog(Action<string> action) : base(action)
        {
            Animation.Add(Fade = new AnimationSlider(0) { Target = 1 });
        }

        public override void Draw(Rect bounds)
        {
            SpriteBatch.DrawRect(bounds, System.Drawing.Color.FromArgb((int)(Fade * 127), 0, 0, 0));
            base.Draw(bounds);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            if (Closing && Fade < 0.05f)
            {
                Close(Output);
            }
            if (Input.KeyTap(Game.Options.General.Binds.Exit) || (!ScreenUtils.MouseOver(GetBounds(bounds)) && Input.MouseClick(OpenTK.Input.MouseButton.Left)))
            {
                OnClosing();
            }
        }

        protected virtual void OnClosing()
        {
            //so dialogs that extend this can also play other animations when closing
            Closing = true;
            Fade.Target = 0f;
        }
    }
}
