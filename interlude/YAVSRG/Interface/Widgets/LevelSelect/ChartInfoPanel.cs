using System.Drawing;
using Prelude.Gameplay.DifficultyRating;
using Interlude.Interface.Animations;
using Interlude.Graphics;

namespace Interlude.Interface.Widgets
{
    public class ChartInfoPanel : FrameContainer
    {
        string time, bpm;
        AnimationColorMixer physical, technical, text;

        public ChartInfoPanel()
        {
            BackColor = () => Utils.ColorInterp(Color.FromArgb(80, 80, 80), Game.Screens.DarkColor, 0.2f);
            AddChild(new TextBox(() => Game.CurrentChart.Data.DiffName, AnchorType.CENTER, 0, true, () => Game.Options.Theme.MenuFont, () => text).BR_DeprecateMe(0, 0.2f, AnchorType.MAX, AnchorType.LERP));
            AddChild(new TextBox(Game.Gameplay.GetModString, AnchorType.CENTER, 0, false, () => Game.Options.Theme.MenuFont, () => text)
                .TL_DeprecateMe(0, 0.15f, AnchorType.MIN, AnchorType.LERP).BR_DeprecateMe(0, 0.3f, AnchorType.MAX, AnchorType.LERP));

            AddChild(new TextBox(() => Utils.RoundNumber(Game.Gameplay.ChartDifficulty.Physical) + "*", AnchorType.MIN, 50f, true, () => Game.Options.Theme.MenuFont, () => physical)
                .TL_DeprecateMe(15, 0.35f, AnchorType.MIN, AnchorType.LERP).BR_DeprecateMe(0, 0.5f, AnchorType.CENTER, AnchorType.LERP));
            AddChild(new TextBox(() => "*" + Utils.RoundNumber(Game.Gameplay.ChartDifficulty.Technical), AnchorType.MAX, 50f, true, () => Game.Options.Theme.MenuFont, () => technical)
                .TL_DeprecateMe(0, 0.35f, AnchorType.CENTER, AnchorType.LERP).BR_DeprecateMe(15, 0.5f, AnchorType.MAX, AnchorType.LERP));

            AddChild(new TextBox(() => "Physical", AnchorType.MIN, 20f, false, () => Game.Options.Theme.MenuFont, () => text)
                .TL_DeprecateMe(15, 0.3f, AnchorType.MIN, AnchorType.LERP).BR_DeprecateMe(0, 0.4f, AnchorType.CENTER, AnchorType.LERP));
            AddChild(new TextBox(() => "Technical", AnchorType.MAX, 20f, false, () => Game.Options.Theme.MenuFont, () => text)
                .TL_DeprecateMe(0, 0.3f, AnchorType.CENTER, AnchorType.LERP).BR_DeprecateMe(15, 0.4f, AnchorType.MAX, AnchorType.LERP));

            //AddChild(new TextBox("Skillset breakdown coming soon", AnchorType.CENTER, 0, false, Game.Options.Theme.MenuFont)
            //    .TL_DeprecateMe(0, 0.5f, AnchorType.MIN, AnchorType.LERP).BR_DeprecateMe(0, 90, AnchorType.MAX, AnchorType.MAX));

            AddChild(new TextBox(() => bpm, AnchorType.MIN, 30f, false, () => Game.Options.Theme.MenuFont, () => text)
                .TL_DeprecateMe(15, 70, AnchorType.MIN, AnchorType.MAX).BR_DeprecateMe(0, 0, AnchorType.CENTER, AnchorType.MAX));
            AddChild(new TextBox(() => time, AnchorType.MAX, 30f, false, () => Game.Options.Theme.MenuFont, () => text)
                .TL_DeprecateMe(0, 70, AnchorType.CENTER, AnchorType.MAX).BR_DeprecateMe(15, 0, AnchorType.MAX, AnchorType.MAX));
            Animation.Add(physical = new AnimationColorMixer(Color.White));
            Animation.Add(technical = new AnimationColorMixer(Color.White));
            Animation.Add(text = new AnimationColorMixer(Color.Black));
            ChangeChart();
        }

        public void ChangeChart()
        {
            time = Utils.FormatTime(Game.CurrentChart.GetDuration() / (float)Game.Options.Profile.Rate);
            bpm = ((int)(Game.CurrentChart.GetBPM() * Game.Options.Profile.Rate)).ToString() + "BPM";
            physical.Target(CalcUtils.PhysicalColor(Game.Gameplay.ChartDifficulty.Physical));
            technical.Target(CalcUtils.TechnicalColor(Game.Gameplay.ChartDifficulty.Physical));
            text.Target(Color.Black);
        }

        public override void Draw(Rect bounds)
        {
            base.Draw(bounds);
            bounds = GetBounds(bounds);
            for (int i = 0; i < 4; i++)
            {
                SpriteBatch.DrawRect(new Rect(bounds.Left + 20, bounds.CenterY + i * 50, bounds.CenterX - 10, bounds.CenterY + 45 + i * 50), Color.FromArgb(127, Game.Screens.DarkColor));
                SpriteBatch.DrawRect(new Rect(bounds.Left + 20, bounds.CenterY + i * 50, bounds.CenterX - 50 - 10 * (i * 7 % 5), bounds.CenterY + 45 + i * 50), Color.FromArgb(80, Game.Screens.BaseColor));
                SpriteBatch.Font2.DrawTextToFill("Placeholder", new Rect(bounds.Left + 20, bounds.CenterY + i * 50, bounds.CenterX - 10, bounds.CenterY + 45 + i * 50), Game.Options.Theme.MenuFont, true, Color.Black);
                SpriteBatch.DrawRect(new Rect(bounds.CenterX + 10, bounds.CenterY + i * 50, bounds.Right - 20, bounds.CenterY + 45 + i * 50), Color.FromArgb(127, Game.Screens.DarkColor));
                SpriteBatch.DrawRect(new Rect(bounds.CenterX + 50 + 10 * (i * 8 % 5), bounds.CenterY + i * 50, bounds.Right - 20, bounds.CenterY + 45 + i * 50), Color.FromArgb(80, Game.Screens.BaseColor));
                SpriteBatch.Font2.DrawJustifiedTextToFill("Placeholder", new Rect(bounds.CenterX + 10, bounds.CenterY + i * 50, bounds.Right - 20, bounds.CenterY + 45 + i * 50), Game.Options.Theme.MenuFont, true, Color.Black);
            }
        }
    }
}
