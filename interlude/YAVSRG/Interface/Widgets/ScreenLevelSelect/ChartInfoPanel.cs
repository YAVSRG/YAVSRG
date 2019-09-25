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
            AddChild(new TextBox(() => Game.CurrentChart.Data.DiffName, TextAnchor.CENTER, 0, true, () => Game.Options.Theme.MenuFont, () => text).Reposition(0, 0, 0, 0, 0, 1, 0, 0.2f));
            AddChild(new TextBox(Game.Gameplay.GetModString, TextAnchor.CENTER, 0, false, () => Game.Options.Theme.MenuFont, () => text)
                .Reposition(0, 0, 0, 0.15f, 0, 1, 0, 0.3f));

            AddChild(new TextBox(() => Utils.RoundNumber(Game.Gameplay.ChartDifficulty.Physical) + "*", TextAnchor.LEFT, 50f, true, () => Game.Options.Theme.MenuFont, () => physical)
                .Reposition(15, 0, 0, 0.35f, 0, 0.5f, 0, 0.5f));
            AddChild(new TextBox(() => "*" + Utils.RoundNumber(Game.Gameplay.ChartDifficulty.Technical), TextAnchor.RIGHT, 50f, true, () => Game.Options.Theme.MenuFont, () => technical)
                .Reposition(0, 0.5f, 0, 0.35f, -15, 1, 0, 0.5f));
            AddChild(new TextBox(() => "Physical", TextAnchor.LEFT, 20f, false, () => Game.Options.Theme.MenuFont, () => text)
                .Reposition(15, 0, 0, 0.3f, 0, 0.5f, 0, 0.4f));
            AddChild(new TextBox(() => "Technical", TextAnchor.RIGHT, 20f, false, () => Game.Options.Theme.MenuFont, () => text)
                .Reposition(0, 0.5f, 0, 0.3f, -15, 1, 0, 0.4f));

            AddChild(new TextBox(() => bpm, TextAnchor.LEFT, 30f, false, () => Game.Options.Theme.MenuFont, () => text)
                .Reposition(15, 0, -70, 1, 0, 0.5f, 0, 1));
            AddChild(new TextBox(() => time, TextAnchor.RIGHT, 30f, false, () => Game.Options.Theme.MenuFont, () => text)
                .Reposition(0, 0.5f, -70, 1, -15, 1, 0, 1));
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
