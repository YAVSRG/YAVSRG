using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Beatmap;

namespace YAVSRG.Gameplay
{
    public class GameplayManager
    {
        //"modded" chart

        public Chart CurrentChart;
        public ChartWithModifiers ModifiedChart;

        public void ChangeChart(Chart c)
        {
            CurrentChart = c;
            Game.Screens.ChangeBackground(Content.LoadBackground(c.path, c.bgpath));
            Game.Audio.ChangeTrack(c.AudioPath());
            Game.Audio.Play((long)c.PreviewTime); //play from the preview point given in the chart data
            UpdateChart();
            //Console.WriteLine(c.GetHash());
        }

        public void UpdateChart()
        {
            ModifiedChart = new ChartWithModifiers(CurrentChart);
            Options.Colorizer.Colorize(ModifiedChart, Game.Options.Profile.ColorStyle);
            //for i in mods
            //mod it
        }

        protected string[] GetBaseModifiers()
        {
            return new string[] { "S" + Game.Options.Profile.ScrollSpeed.ToString() + "x", "R" + Game.Options.Profile.Rate.ToString() + "x", Game.Options.Profile.Skin };
        }

        public string[] GetModifiers()
        {
            return GetBaseModifiers();
        }
    }
}
