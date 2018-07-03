using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;
using System.IO;
using YAVSRG.Gameplay.Mods;
using YAVSRG.Charts.DifficultyRating;

namespace YAVSRG.Gameplay
{
    public class GameplayManager
    {
        public Dictionary<string, Mod> mods = new Dictionary<string, Mod>() { { "Auto", new AutoPlay() }, { "NoLN", new NoLN() }, { "Mirror", new Mirror() }, { "NoSV", new NoSV() } };

        public Chart CurrentChart;
        public Charts.CachedChart CurrentCachedChart;
        public ChartWithModifiers ModifiedChart;
        public RatingReport ChartDifficulty;
        public ChartSaveData ChartSaveData;
        public Dictionary<string, string> SelectedMods = new Dictionary<string, string>();
        public event Action OnUpdateChart = () => { };

        public ScoresDB ScoreDatabase = ScoresDB.Load();

        public void ChangeChart(Charts.CachedChart cache, Chart c, bool playFromPreview)
        {
            CurrentCachedChart = cache;
            CurrentChart = c;
            Game.Screens.ChangeBackground(Content.LoadBackground(c.Data.SourcePath, c.Data.BGFile));
            Game.Audio.ChangeTrack(c.AudioPath());
            if (playFromPreview)
            {
                Game.Audio.Play((long)c.Data.PreviewTime); //play from the preview point given in the chart data
            }
            else
            {
                Game.Audio.Play();
            }
            ChartSaveData = ScoreDatabase.GetChartSaveData(CurrentChart);
            UpdateChart();
        }

        public void UpdateChart()
        {
            ModifiedChart = GetModifiedChart(SelectedMods);
            Options.Colorizer.Colorize(ModifiedChart, Game.Options.Profile.ColorStyle);
            UpdateDifficulty();
            OnUpdateChart();
        }

        public void UpdateDifficulty()
        {
            ChartDifficulty = new RatingReport(ModifiedChart, (float)Game.Options.Profile.Rate, Game.Options.Profile.Playstyles[ModifiedChart.Keys]);
        }

        public void Unload()
        {
            CurrentChart = null;
            ModifiedChart = null;
            ChartSaveData = null;
        }

        public float GetChartOffset()
        {
            return ChartSaveData.Offset - CurrentChart.Notes.Points[0].Offset;
        }

        public void ApplyModsToHitData(ChartWithModifiers c, ref ScoreTracker.HitData[] hitdata)
        {
            foreach (string m in SelectedMods.Keys)
            {
                if (mods[m].IsApplicable(c, SelectedMods[m]))
                {
                    mods[m].ApplyToHitData(c, ref hitdata, SelectedMods[m]);
                }
            }
        }

        public ChartWithModifiers GetModifiedChart(Dictionary<string,string> SelectedMods)
        {
            ChartWithModifiers c = new ChartWithModifiers(CurrentChart);
            foreach (string m in SelectedMods.Keys)
            {
                if (mods[m].IsApplicable(c, SelectedMods[m]))
                {
                    mods[m].Apply(c, SelectedMods[m]);
                }
            }
            return c;
        }

        public int GetModStatus(Dictionary<string,string> SelectedMods)
        {
            int s = 0;
            foreach (string m in SelectedMods.Keys)
            {
                s = Math.Max(s, mods[m].GetStatus(SelectedMods[m]));
            }
            return s;
        }

        public string GetModString(Dictionary<string, string> SelectedMods, float rate, string playstyle)
        {
            string result = Utils.RoundNumber(rate) + "x, " + playstyle;
            foreach (string m in SelectedMods.Keys)
            {
                if (mods[m].IsApplicable(ModifiedChart, SelectedMods[m]))
                {
                    result += ", "+mods[m].GetName(SelectedMods[m]);
                }
            }
            return result;
        }

        public void SaveScores()
        {
            ScoreDatabase.Save();
        }
    }
}
