using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using YAVSRG.Interface.Animations;
using YAVSRG.Gameplay.DifficultyRating;

namespace YAVSRG.Interface.Widgets
{
    public class ChartInfoPanel : FrameContainer
    {
        string time, bpm;
        AnimationColorMixer physical, technical, text;

        public ChartInfoPanel()
        {
            AddChild(new TextBox(() => Game.CurrentChart.Data.DiffName, AnchorType.CENTER, 0, true, () => Game.Options.Theme.MenuFont, () => text).PositionBottomRight(0, 0.2f, AnchorType.MAX, AnchorType.LERP));
            AddChild(new TextBox(Game.Gameplay.GetModString, AnchorType.CENTER, 0, false, () => Game.Options.Theme.MenuFont, () => text)
                .PositionTopLeft(0, 0.15f, AnchorType.MIN, AnchorType.LERP).PositionBottomRight(0, 0.3f, AnchorType.MAX, AnchorType.LERP));

            AddChild(new TextBox(() => Utils.RoundNumber(Game.Gameplay.ChartDifficulty.Physical) + "*", AnchorType.MIN, 50f, true, () => Game.Options.Theme.MenuFont, () => physical)
                .PositionTopLeft(15, 0.35f, AnchorType.MIN, AnchorType.LERP).PositionBottomRight(0, 0.5f, AnchorType.CENTER, AnchorType.LERP));
            AddChild(new TextBox(() => "*" + Utils.RoundNumber(Game.Gameplay.ChartDifficulty.Technical), AnchorType.MAX, 50f, true, () => Game.Options.Theme.MenuFont, () => technical)
                .PositionTopLeft(0, 0.35f, AnchorType.CENTER, AnchorType.LERP).PositionBottomRight(15, 0.5f, AnchorType.MAX, AnchorType.LERP));

            AddChild(new TextBox(() => "Physical", AnchorType.MIN, 20f, false, () => Game.Options.Theme.MenuFont, () => text)
                .PositionTopLeft(15, 0.3f, AnchorType.MIN, AnchorType.LERP).PositionBottomRight(0, 0.4f, AnchorType.CENTER, AnchorType.LERP));
            AddChild(new TextBox(() => "Technical", AnchorType.MAX, 20f, false, () => Game.Options.Theme.MenuFont, () => text)
                .PositionTopLeft(0, 0.3f, AnchorType.CENTER, AnchorType.LERP).PositionBottomRight(15, 0.4f, AnchorType.MAX, AnchorType.LERP));

            AddChild(new TextBox("Skillset breakdown coming soon", AnchorType.CENTER, 0, false, Game.Options.Theme.MenuFont)
                .PositionTopLeft(0, 0.5f, AnchorType.MIN, AnchorType.LERP).PositionBottomRight(0, 90, AnchorType.MAX, AnchorType.MAX));

            AddChild(new TextBox(() => bpm, AnchorType.MIN, 30f, false, () => Game.Options.Theme.MenuFont, () => text)
                .PositionTopLeft(15, 70, AnchorType.MIN, AnchorType.MAX).PositionBottomRight(0, 0, AnchorType.CENTER, AnchorType.MAX));
            AddChild(new TextBox(() => time, AnchorType.MAX, 30f, false, () => Game.Options.Theme.MenuFont, () => text)
                .PositionTopLeft(0, 70, AnchorType.CENTER, AnchorType.MAX).PositionBottomRight(15, 0, AnchorType.MAX, AnchorType.MAX));
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
    }
}
