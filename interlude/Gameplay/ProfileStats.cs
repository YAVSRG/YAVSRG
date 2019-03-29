using System;
using System.Collections.Generic;
using Prelude.Gameplay.DifficultyRating;
using Prelude.Gameplay;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Utilities;

namespace Interlude.Gameplay
{
    public class ProfileStats
    {
        //todo: move this to profile namespace
        public int SecondsPlayed, TimesPlayed, TimesQuit, SRanks;

        public List<TopScore>[] PhysicalBest = new List<TopScore>[8], TechnicalBest = new List<TopScore>[8];

        public float[] PhysicalMean = new float[8], TechnicalMean = new float[8];

        public void UpdateMeans(int i)
        {
            TechnicalMean[i] = 1;
            if (PhysicalBest[i] != null)
            {
                PhysicalMean[i] = 0;
                foreach (TopScore t in PhysicalBest[i])
                {
                    PhysicalMean[i] += t.Rating;
                }
                PhysicalMean[i] /= 50f;
            }
        }

        public void SetScore(Score Score, Chart Chart)
        {
            //create score tables if not already present
            int k = Score.keycount - 3;

            if (PhysicalBest[k] == null)
            {
                PhysicalBest[k] = new List<TopScore>();
            }
            if (TechnicalBest[k] == null)
            {
                TechnicalBest[k] = new List<TopScore>();
            }

            List<TopScore> KeymodeScores = PhysicalBest[k];
            var HitData = ScoreTracker.StringToHitData(Score.hitdata, Score.keycount);
            ChartWithModifiers ModdedChart = Game.Gameplay.GetModifiedChart(Score.mods, Chart);
            float ScoreRating = PlayerRating.GetRating(new RatingReport(ModdedChart, Score.rate, Score.layout), HitData);
            TopScore NewTopScore = new TopScore(Chart.GetFileIdentifier(), Game.Gameplay.ScoreDatabase.GetChartSaveData(Chart).Scores.IndexOf(Score), ScoreRating); //score is added to list after this function is over, so .Count gives correct id
            bool inserted = false;

            //look through existing top scores (earlier = higher rating)
            for (int i = 0; i < KeymodeScores.Count; i++)
            {
                if (KeymodeScores[i].FileIdentifier == NewTopScore.FileIdentifier) //if there is already a score on this file
                {
                    if (ScoreRating > KeymodeScores[i].Rating) //if this is a new best, remove it
                    {
                        KeymodeScores.RemoveAt(i);
                    }
                    else //(otherwise do nothing)
                    {
                        inserted = true;
                    }
                    break; //score has now been handled
                }
            }
            if (!inserted)
            {
                for (int i = 0; i < KeymodeScores.Count; i++)
                {
                    if (KeymodeScores[i].Rating < ScoreRating) //find a score below this one
                    {
                        inserted = true;
                        KeymodeScores.Insert(i, NewTopScore); //insert it here
                        break; //score has now been handled
                    }
                }
            }
            if (!inserted) //if we couldn't find a place for the score
            {
                KeymodeScores.Add(NewTopScore); //put it at the end
            }
            if (KeymodeScores.Count > 50)
            {
                KeymodeScores.RemoveAt(50); //remove a score if there are more than 50 now
            }
            UpdateMeans(k);
        }

        public IEnumerable<ScoreInfoProvider> GetPhysicalTop(int i)
        {
            if (PhysicalBest[i] == null) yield break;
            Score s = null; Chart c = null;
            foreach (TopScore score in PhysicalBest[i])
            {
                try
                {
                    c = ChartLoader.Cache.LoadChart(ChartLoader.Cache.Charts[score.FileIdentifier]);
                    s = Game.Gameplay.ScoreDatabase.GetChartSaveData(c).Scores[score.ScoreID];
                }
                catch (Exception e)
                {
                    Logging.Log("Could not retrieve score data from " + score.FileIdentifier, e.ToString(), Logging.LogType.Error);
                    continue;
                }
                yield return new ScoreInfoProvider(s, c);
            }
        }

        public IEnumerable<ScoreInfoProvider> GetTechnicalTop(int i)
        {
            yield break; //nyi
        }

        public Utilities.TaskManager.UserTask RecalculateTop()
        {
            return (Output) =>
            {
                PhysicalBest = new List<TopScore>[8];
                TechnicalBest = new List<TopScore>[8];
                List<string> oldHashes = new List<string>();
                lock (Game.Gameplay.ScoreDatabase.data) //nothing really interferes but this is just in case
                {
                    foreach (string hash in Game.Gameplay.ScoreDatabase.data.Keys)
                    {
                        ChartSaveData d = Game.Gameplay.ScoreDatabase.data[hash];
                        if (!ChartLoader.Cache.Charts.ContainsKey(d.Path))
                        {
                            Logging.Log("Found score data for " + d.Path + " - the file no longer exists; will be deleted");
                            oldHashes.Add(hash);
                            continue;
                        }
                        Chart c = ChartLoader.Cache.LoadChart(ChartLoader.Cache.Charts[d.Path]);
                        if (c.GetHash() != hash)
                        {
                            Logging.Log("Found old score data for " + d.Path + " - the file is different; will be deleted");
                            oldHashes.Add(hash);
                            continue;
                        }
                        foreach (Score s in d.Scores)
                        {
                            if (s.playerUUID == Game.Options.Profile.UUID)
                                SetScore(s, c);
                        }
                    }
                    foreach (string hash in oldHashes)
                    {
                        Game.Gameplay.ScoreDatabase.data.Remove(hash);
                    }
                }
                return true;
            };
        }
    }
}
