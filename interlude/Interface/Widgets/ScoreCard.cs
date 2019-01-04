using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    public class ScoreCard : FrameContainer
    {
        ScoreInfoProvider Data;

        public ScoreCard(ScoreInfoProvider data)
        {
            PositionBottomRight(0, 100, AnchorType.MAX, AnchorType.MIN);
            Data = data;
            AddChild(new TextBox(Data.Player, AnchorType.MIN, 0, true, Game.Options.Theme.MenuFont, Color.Black).PositionBottomRight(0.5f, 0.6f, AnchorType.LERP, AnchorType.LERP));
            AddChild(new TextBox(Data.Mods, AnchorType.MIN, 0, false, Game.Options.Theme.MenuFont, Color.Black).PositionTopLeft(0f, 0.6f, AnchorType.LERP,AnchorType.LERP).PositionBottomRight(0.6f, 1f, AnchorType.LERP, AnchorType.LERP));
            AddChild(new TextBox(Data.Accuracy, AnchorType.MAX, 0, true, Game.Options.Theme.MenuFont, Color.Black).PositionTopLeft(0.5f, 0f, AnchorType.LERP, AnchorType.LERP).PositionBottomRight(1f, 0.6f, AnchorType.LERP, AnchorType.LERP));
            AddChild(new TextBox(Utils.RoundNumber(Data.PhysicalPerformance)+" // "+Data.Time.ToShortDateString(), AnchorType.MAX, 0, false, Game.Options.Theme.MenuFont, Color.Black).PositionTopLeft(0.6f, 0.6f, AnchorType.LERP, AnchorType.LERP).PositionBottomRight(1f, 1f, AnchorType.LERP, AnchorType.LERP));
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.MouseOver(bounds))
            {
                if (Input.MouseClick(OpenTK.Input.MouseButton.Left))
                {
                    Game.Screens.AddDialog(new Dialogs.ScoreInfoDialog(Data, (s) => { }));
                }
                else if (Input.MouseClick(OpenTK.Input.MouseButton.Right) && Input.KeyPress(OpenTK.Input.Key.Delete))
                {
                    SetState(0);
                    //todo: find way to support this given that this changes score index for use in top scores system
                    //Game.Gameplay.ChartSaveData.Scores.Remove(c);
                }
            }
        }

        public static Comparison<Widget> Compare = (a, b) => { return ((ScoreCard)b).Data.PhysicalPerformance.CompareTo(((ScoreCard)a).Data.PhysicalPerformance); };
    }
}
