using System;
using System.Drawing;
using Interlude.Gameplay;
using Interlude.IO;

namespace Interlude.Interface.Widgets
{
    public class ScoreCard : FrameContainer
    {
        ScoreInfoProvider Data;

        public ScoreCard(ScoreInfoProvider data)
        {
            Reposition(0, 0, 0, 0, 0, 1, 100, 0);
            Data = data;
            AddChild(new TextBox(Data.Player, TextAnchor.LEFT, 0, true, Game.Options.Theme.MenuFont, Color.Black).Reposition(0, 0, 0, 0, 0, 0.5f, 0, 0.6f));
            AddChild(new TextBox(Data.Mods, TextAnchor.LEFT, 0, false, Game.Options.Theme.MenuFont, Color.Black).Reposition(0, 0, 0, 0.6f, 0, 0.6f, 0, 1));
            AddChild(new TextBox(Data.ScoreShorthand, TextAnchor.RIGHT, 0, true, Game.Options.Theme.MenuFont, Color.Black).Reposition(0, 0.5f, 0, 0, 0, 1, 0, 0.6f));
            AddChild(new TextBox(Utils.RoundNumber(Data.PhysicalPerformance) + " // " + Data.Time.ToShortDateString(), TextAnchor.RIGHT, 0, false, Game.Options.Theme.MenuFont, Color.Black).Reposition(0, 0.6f, 0, 0.6f, 0, 1, 0, 1));
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
