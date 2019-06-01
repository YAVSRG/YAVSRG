using System;
using System.Collections.Generic;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay.Mods;
using Prelude.Gameplay.DifficultyRating;
using Prelude.Gameplay;
using Prelude.Utilities;
using Interlude.Gameplay.Charts.Collections;
using Interlude.IO;

namespace Interlude.Gameplay
{
    //Manages the selected chart, its associated data (scores and local offset) and applying selected modifiers to it
    public class GameplayManager
    {
        public Dictionary<string, Mod> Mods = new Dictionary<string, Mod>() { { "Auto", new AutoPlay() }, { "NoLN", new NoLN() }, { "Random", new Randomise() }, { "Manipulate", new Manipulate() }, { "Mirror", new Mirror() }, { "NoSV", new NoSV() }, { "Wave", new Wave() } };

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
            /*if (Game.Multiplayer.Connected)
            {
                Game.Multiplayer.SendPacket(new Net.P2P.Protocol.Packets.PacketPlay() {
                    diff = CurrentChart.Data.DiffName, mods = SelectedMods, rate = (float)Game.Options.Profile.Rate,
                    hash = CurrentChart.GetHash(), name = CurrentChart.Data.Title, pack = CurrentChart.Data.SourcePack });
            }*/
        }

        public void ApplyModsToHitData(ChartWithModifiers c, ref HitData[] hitdata)
        {
            foreach (string m in SelectedMods.Keys)
            {
                if (Mods[m].IsApplicable(c, SelectedMods[m]))
                {
                    Mods[m].ApplyToHitData(c, ref hitdata, SelectedMods[m]);
                }
            }
        }

        public ChartWithModifiers GetModifiedChart(Dictionary<string, DataGroup> Mods, Chart Base)
        {
            ChartWithModifiers c = new ChartWithModifiers(Base);
            foreach (string m in Mods.Keys)
            {
                if (this.Mods[m].IsApplicable(c, Mods[m]))
                {
                    this.Mods[m].Apply(c, Mods[m]);
                    c.Mods = this.Mods[m].GetName(Mods[m]);
                    c.ModStatus = Math.Max(c.ModStatus, this.Mods[m].GetStatus(Mods[m]));
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
