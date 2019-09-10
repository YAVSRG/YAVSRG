using System.Drawing;
using Prelude.Gameplay;
using Prelude.Gameplay.DifficultyRating;
using Interlude.Gameplay;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class TopScoreCard : Widget
    {
        ScoreInfoProvider Data;
        bool Light;

        public TopScoreCard(ScoreInfoProvider data, bool light, bool technical)
        {
            Data = data;
            Light = light;

            AddChild(new TextBox(data.Data.Title, AnchorType.MIN, 0, true, Game.Options.Theme.MenuFont, Color.Black)
                .Reposition(0, 0, 0, 0, 0, 0.4f, 0, 0.6f));
            AddChild(new TextBox(data.Data.DiffName + " // " + data.Data.Creator, AnchorType.MIN, 0, false, Game.Options.Theme.MenuFont, Color.Black)
                .Reposition(0, 0, 0, 0.6f, 0, 0.4f, 0, 1));
            AddChild(new TextBox(data.Mods, AnchorType.CENTER, 0, true, Game.Options.Theme.MenuFont, Color.Black)
                .Reposition(0, 0.4f, 0, 0, 0, 0.8f, 0, 0.6f));
            AddChild(new TextBox(data.Time.ToString(), AnchorType.CENTER, 0, false, Game.Options.Theme.MenuFont, Color.Black)
                .Reposition(0, 0.4f, 0, 0.6f, 0, 0.8f, 0, 1));
            AddChild(new TextBox(Utils.RoundNumber(data.PhysicalPerformance), AnchorType.CENTER, 0, true, Game.Options.Theme.MenuFont, CalcUtils.PhysicalColor(data.PhysicalPerformance))
                .Reposition(0, 0.8f, 0, 0, 0, 1, 0, 0.6f));
            AddChild(new TextBox(data.FormattedAccuracy, AnchorType.CENTER, 0, false, Game.Options.Theme.MenuFont, Color.Black)
                .Reposition(0, 0.8f, 0, 0.6f, 0, 1, 0, 1));
        }

        public override void Draw(Rect bounds)
        {
            bounds = GetBounds(bounds);
            SpriteBatch.DrawRect(bounds, Color.FromArgb(127, Light ? Color.DimGray : Color.Black));
            DrawWidgets(bounds);
        }

        /*
        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            bounds = GetBounds(bounds);
            if (ScreenUtils.MouseOver(bounds))
            {
                if (accdisplay != "???%" && Input.MouseClick(OpenTK.Input.MouseButton.Left))
                {
                    Game.Screens.AddDialog(new Dialogs.ScoreInfoDialog(c, (s) => { }));
                }
                else if (Input.MouseClick(OpenTK.Input.MouseButton.Right) && Input.KeyPress(OpenTK.Input.Key.Delete))
                {
                    SetState(0);
                    Game.Gameplay.ChartSaveData.Scores.Remove(c);
                }
            }
        }*/
    }
}
