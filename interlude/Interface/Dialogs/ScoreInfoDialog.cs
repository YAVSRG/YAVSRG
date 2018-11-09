using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Interface.Animations;
using YAVSRG.Gameplay;
using YAVSRG.Gameplay.Watchers;

namespace YAVSRG.Interface.Dialogs
{
    public class ScoreInfoDialog : FadeDialog
    {
        ScoreInfoProvider Data;

        public ScoreInfoDialog(ScoreInfoProvider data, Action<string> a) : base(a)
        {
            PositionTopLeft(-ScreenUtils.ScreenWidth * 2 + 200, 200, AnchorType.MIN, AnchorType.MIN).PositionBottomRight(ScreenUtils.ScreenWidth * 2 + 200, 200, AnchorType.MAX, AnchorType.MAX);
            Move(new Rect(200, 200, 200, 200));

            Data = data;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            Game.Screens.DrawChartBackground(bounds, Game.Screens.DarkColor, 1f);
            ScreenUtils.DrawFrame(bounds, 30f, System.Drawing.Color.White);

            //ScreenUtils.DrawGraph(new Rect(bounds.Left + 20, bounds.Bottom - 200, bounds.Right - 20, bounds.Bottom - 20), scoring, data);
        }

        protected override void OnClosing()
        {
            base.OnClosing();
            Move(new Rect(-ScreenUtils.ScreenWidth * 2 + 200, 200, ScreenUtils.ScreenWidth * 2 + 200, 200));
        }
    }
}
