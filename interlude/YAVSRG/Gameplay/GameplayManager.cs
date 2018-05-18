using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;
using System.IO;
using YAVSRG.Gameplay.Mods;

namespace YAVSRG.Gameplay
{
    public class GameplayManager
    {
        //"modded" chart
        public Mod[] mods = new Mod[] { new Mirror(), new NoSV(), new Inverse() };

        public Chart CurrentChart;
        public ChartWithModifiers ModifiedChart;
        public ChartSaveData ChartSaveData;
        public event Action OnUpdateChart = () => { };

        public void ChangeChart(Chart c)
        {
            if (CurrentChart != null)
            {
                Utils.SaveObject(ChartSaveData, Path.Combine(Content.WorkingDirectory, "Data", "Scores", CurrentChart.GetHash() + ".json"));
            }
            CurrentChart = c;
            Game.Screens.ChangeBackground(Content.LoadBackground(c.Data.SourcePath, c.Data.BGFile));
            Game.Audio.ChangeTrack(c.AudioPath());
            Game.Audio.Play((long)c.Data.PreviewTime); //play from the preview point given in the chart data
            ChartSaveData = GetChartSaveData();
            UpdateChart();
            //Console.WriteLine(c.GetHash());
        }

        public void UpdateChart()
        {
            ModifiedChart = new ChartWithModifiers(CurrentChart);
            //for i in mods
            foreach (Mod m in mods)
            {
                if (m.IsActive(ModifiedChart))
                {
                    m.Apply(ModifiedChart);
                }
            }
            Options.Colorizer.Colorize(ModifiedChart, Game.Options.Profile.ColorStyle);
            OnUpdateChart();
            //mod it
        }

        public void Unload()
        {
            if (CurrentChart != null)
            {
                Utils.SaveObject(ChartSaveData, Path.Combine(Content.WorkingDirectory, "Data", "Scores", CurrentChart.GetHash() + ".json"));
            }
            CurrentChart = null;
            ModifiedChart = null;
            ChartSaveData = null;
        }

        public float GetChartOffset()
        {
            return ChartSaveData.Offset - CurrentChart.Notes.Points[0].Offset;
        }

        protected ChartSaveData GetChartSaveData()
        {
            string absolutePath = Path.Combine(Content.WorkingDirectory, "Data", "Scores", CurrentChart.GetHash() + ".json");
            if (File.Exists(absolutePath))
            {
                return Utils.LoadObject<ChartSaveData>(absolutePath);
            }
            return ChartSaveData.FromChart(CurrentChart);
        }

        protected string[] GetBaseModifiers()
        {
            return new string[] { Game.Options.Profile.Rate.ToString("0.0#") + "x" };
        }

        public void EmulateModifiers(string[] mods)
        {
            //stub
        }

        public string[] GetModifiers()
        {
            List<string> l = GetBaseModifiers().ToList();
            foreach (Mod m in mods)
            {
                if (m.IsActive(ModifiedChart))
                {
                    l.Add(m.GetName());
                }
            }
            return l.ToArray();
        }
    }
}
