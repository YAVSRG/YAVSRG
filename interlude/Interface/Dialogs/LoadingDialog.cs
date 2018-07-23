using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;
using YAVSRG.Charts;

namespace YAVSRG.Interface.Dialogs
{
    public class LoadingDialog : Dialog
    {
        AnimationColorMixer c;
        AnimationCounter anim;
        public LoadingDialog(Action<string> action) : base(action)
        {
            Animation.Add(c = new AnimationColorMixer(System.Drawing.Color.FromArgb(0, 0, 0, 0)));
            Animation.Add(anim = new AnimationCounter(100000000, true));
            c.Target(System.Drawing.Color.FromArgb(200, 0, 0, 0));
            SpriteBatch.ParallelogramTransform(0, 0);
        }

        public override void Update(float left, float top, float right, float bottom)
        {
            base.Update(left, top, right, bottom);
            if (ChartLoader.LastStatus != ChartLoader.ChartLoadingStatus.InProgress)
            {
                Close("");
            }
        }

        public override void Draw(float left, float top, float right, float bottom)
        {
            base.Draw(left, top, right, bottom);
            ConvertCoordinates(ref left, ref top, ref right, ref bottom);
            SpriteBatch.DrawRect(left, top, right, bottom, c);
            SpriteBatch.Font1.DrawCentredTextToFill("Loading...", -300, top + 100, 300, top + 300, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredTextToFill(ChartLoader.LastOutput, left, -400, right, 0, Game.Options.Theme.MenuFont);
            SpriteBatch.Font1.DrawCentredTextToFill("You can press " + Game.Options.General.Binds.CollapseToToolbar.ToString().ToUpper() + " to hide the game to the taskbar and do something else while you wait. Such as minesweeper...", left, -200, right, 200, Game.Options.Theme.MenuFont);
            ScreenUtils.DrawLoadingAnimation(100f, 0, 200, anim.value * 0.01f);
        }
    }
}
