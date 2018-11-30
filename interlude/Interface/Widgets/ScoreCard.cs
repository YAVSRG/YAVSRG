using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Gameplay;
using System.Drawing;

namespace YAVSRG.Interface.Widgets
{
    public class ScoreCard : Widget
    {
        ScoreInfoProvider Data;

        public ScoreCard(ScoreInfoProvider data)
        {
            PositionBottomRight(0, 100, AnchorType.MAX, AnchorType.MIN);
            Data = data;
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            ScreenUtils.DrawFrame(bounds, 30f, Game.Screens.HighlightColor);
            SpriteBatch.Font1.DrawText(Data.Player, 35f, bounds.Left + 5, bounds.Top, Game.Options.Theme.MenuFont, true, Color.Black);
            SpriteBatch.Font2.DrawTextToFill(Data.Mods, new Rect(bounds.Left, bounds.Bottom - 40,
                bounds.Right - SpriteBatch.Font2.DrawJustifiedText(Data.Time.ToString(), 20f, bounds.Right - 5, bounds.Bottom - 35, Game.Options.Theme.MenuFont, true, Color.Black) - 10,
                bounds.Bottom), Game.Options.Theme.MenuFont, true, Color.Black);

            SpriteBatch.Font1.DrawJustifiedText(Data.Accuracy, 35f, bounds.Right - 5, bounds.Top, Game.Options.Theme.MenuFont, true, Color.Black);
            SpriteBatch.Font2.DrawJustifiedText(Utils.RoundNumber(Data.PhysicalPerformance), 20f, bounds.Right - 5, bounds.Bottom - 60, Game.Options.Theme.MenuFont, true, Color.Black);
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
