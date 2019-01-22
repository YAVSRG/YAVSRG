using System;
using YAVSRG.Gameplay.DifficultyRating;
using YAVSRG.Graphics;

namespace YAVSRG.Interface.Widgets.Toolbar
{
    public class ProfileInfoPanel : ToolbarWidget
    {
        public ProfileInfoPanel()
        {
            /*
            AddChild(new TextBox(() => Game.Options.Profile.Name, AnchorType.MIN, 0, true, () => Game.Options.Theme.MenuFont, () => Game.Screens.DarkColor)
                .PositionTopLeft(0, 80, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(400, 30, AnchorType.MIN, AnchorType.MAX));
            AddChild(new TextBox(() => Utils.RoundNumber(Game.Options.Profile.Stats.PhysicalMean[Game.CurrentChart.Keys - 3]), AnchorType.MIN, 0, true, () => Game.Options.Theme.MenuFont, () => Utils.PhysicalColor(Game.Options.Profile.Stats.PhysicalMean[Game.CurrentChart.Keys - 3]))
                .PositionTopLeft(0, 40, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(60, 00, AnchorType.MIN, AnchorType.MAX));
            AddChild(new TextBox(() => Utils.RoundNumber(Game.Options.Profile.Stats.TechnicalMean[Game.CurrentChart.Keys - 3]), AnchorType.MIN, 0, true, () => Game.Options.Theme.MenuFont, () => Utils.TechnicalColor(Game.Options.Profile.Stats.PhysicalMean[Game.CurrentChart.Keys - 3]))
                .PositionTopLeft(60, 40, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(120, 00, AnchorType.MIN, AnchorType.MAX));*/
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            float x = Math.Max(250, Math.Min(800, SpriteBatch.Font1.MeasureText(Game.Options.Profile.Name, 35))) + 90;
            SpriteBatch.Font1.DrawTextToFill(Game.Options.Profile.Name, new Rect(bounds.Left, bounds.Bottom - 82, bounds.Left + x - 90, bounds.Bottom - 20), Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
            SpriteBatch.Font1.DrawJustifiedText(Utils.RoundNumber(Game.Options.Profile.Stats.PhysicalMean[Game.CurrentChart.Keys - 3]), 30f, bounds.Left + x, bounds.Bottom - 80, Game.Options.Theme.MenuFont, true, CalcUtils.PhysicalColor(Game.Options.Profile.Stats.PhysicalMean[Game.CurrentChart.Keys - 3]));
            SpriteBatch.Font1.DrawJustifiedText(Utils.RoundNumber(Game.Options.Profile.Stats.TechnicalMean[Game.CurrentChart.Keys - 3]), 30f, bounds.Left + x, bounds.Bottom - 45, Game.Options.Theme.MenuFont, true, CalcUtils.PhysicalColor(Game.Options.Profile.Stats.TechnicalMean[Game.CurrentChart.Keys - 3]));
            SpriteBatch.Font2.DrawTextToFill("Click here to change profile...", new Rect(bounds.Left + 10, bounds.Bottom - 30, bounds.Left + x, bounds.Bottom), Game.Options.Theme.MenuFont, true, Game.Screens.DarkColor);
        }

        public override void Update(Rect bounds)
        {
            base.Update(bounds);
            float x = Math.Max(250, Math.Min(800, SpriteBatch.Font1.MeasureText(Game.Options.Profile.Name, 35))) + 90;
            if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left + x - 90, bounds.Bottom - 80, bounds.Left + x, bounds.Bottom - 40)))
            {
                Game.Screens.AddDialog(new Dialogs.TopScoreDialog(false));
            }
            else if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left + x - 90, bounds.Bottom - 40, bounds.Left + x, bounds.Bottom)))
            {
                Game.Screens.AddDialog(new Dialogs.TopScoreDialog(true));
            }
            else if (ScreenUtils.CheckButtonClick(new Rect(bounds.Left, bounds.Bottom - 80, bounds.Left + x - 90, bounds.Bottom)))
            {
                Game.Screens.AddDialog(new Dialogs.ProfileDialog((s) => { }));
            }
        }
    }
}
