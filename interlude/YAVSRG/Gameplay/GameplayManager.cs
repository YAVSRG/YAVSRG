using System;
using System.Collections.Generic;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay.Mods;
using Prelude.Gameplay.DifficultyRating;
using Prelude.Gameplay;
using Prelude.Utilities;
using Interlude.Gameplay.Collections;
using Interlude.IO;

namespace Interlude.Gameplay
{
    //Manages the selected chart, its associated data (scores and local offset) and applying selected modifiers to it
    public class GameplayManager
    {
        public Chart CurrentChart;
        public CachedChart CurrentCachedChart;
        public ChartWithModifiers ModifiedChart;
        public RatingReport ChartDifficulty;
        public ChartSaveData ChartSaveData;
        public CollectionsManager Collections = CollectionsManager.LoadCollections();
        public Dictionary<string, DataGroup> SelectedMods = new Dictionary<string, DataGroup>();
        public event Action OnUpdateChart = () => { };

        public ScoresDB ScoreDatabase = ScoresDB.Load();

        public void ChangeChart(CachedChart cache, Chart c, bool playFromPreview)
        {
            CurrentCachedChart = cache;
            if (cache.collection != null)
            {
                Collections.GetCollection(cache.collection).GetPlaylistData(cache.collectionIndex)?.Apply();
            }
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
            if (CurrentChart == null) return;
            ModifiedChart = GetModifiedChart(SelectedMods, CurrentChart);
            Options.Colorizer.Colorize(ModifiedChart, Game.Options.Profile.ColorStyle);
            UpdateDifficulty();
            OnUpdateChart();
        }

        public void UpdateDifficulty()
        {
            ChartDifficulty = new RatingReport(ModifiedChart, (float)Game.Options.Profile.Rate, Game.Options.Profile.KeymodeLayouts[ModifiedChart.Keys]);
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

        public void PlaySelectedChart()
        {
            Game.Screens.AddScreen(new Interface.Screens.ScreenPlay());
        }

        public void ApplyModsToHitData(ChartWithModifiers c, ref HitData[] hitdata)
        {
            foreach (string m in SelectedMods.Keys)
            {
                if (Mod.AvailableMods[m].IsApplicable(c, SelectedMods[m]))
                {
                    Mod.AvailableMods[m].ApplyToHitData(c, ref hitdata, SelectedMods[m]);
                }
            }
        }

        public ChartWithModifiers GetModifiedChart(Dictionary<string, DataGroup> SelectedMods, Chart Base)
        {
            ChartWithModifiers c = new ChartWithModifiers(Base);
            foreach (string m in Mod.AvailableMods.Keys)
            {
                if (SelectedMods.ContainsKey(m) && Mod.AvailableMods[m].IsApplicable(c, SelectedMods[m]))
                {
                    Mod.AvailableMods[m].Apply(c, SelectedMods[m]);
                    c.Mods = Mod.AvailableMods[m].Name;
                    c.ModStatus = Math.Max(c.ModStatus, Mod.AvailableMods[m].Status);
                }
            }
            return c;
        }

        public string GetModString(ChartWithModifiers chart, float rate, KeyLayout.Layout layout)
        {
            return Utils.RoundNumber(rate) + "x, " + KeyLayout.GetLayoutName(layout, chart.Keys) + chart.Mods;
        }

        public string GetModString()
        {
            return GetModString(ModifiedChart, (float)Game.Options.Profile.Rate, Game.Options.Profile.KeymodeLayouts[ModifiedChart.Keys]);
        }

        public void SaveScores()
        {
            ScoreDatabase.Save();
        }
    }
}
