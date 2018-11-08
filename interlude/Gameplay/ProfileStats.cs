using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Gameplay
{
    public class ProfileStats
    {
        public int SecondsPlayed;
        public int TimesPlayed;
        //public int NotesHit;
        public int TimesQuit;
        public int SRanks;

        public List<TopScore>[] PhysicalBest = new List<TopScore>[8];
        public List<TopScore>[] TechnicalBest = new List<TopScore>[8];

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
            float ScoreRating = Charts.DifficultyRating.PlayerRating.GetRating(new Charts.DifficultyRating.RatingReport(ModdedChart, Score.rate, Score.playstyle), HitData);
            TopScore NewTopScore = new TopScore(Chart.GetFileIdentifier(), Game.Gameplay.ScoreDatabase.GetChartSaveData(Chart).Scores.IndexOf(Score), ScoreRating); //score is added to list after this function is over, so .Count gives correct id
            bool inserted = false;

            //look through existing top scores (earlier = higher rating)
            for (int i = 0; i < KeymodeScores.Count; i++)
            {
                if (KeymodeScores[i].FileIdentifier == NewTopScore.FileIdentifier) //if there is already a score on this file
                {
                    if (ScoreRating > KeymodeScores[i].Rating) //if this is a new best, edit it to this score
                    {
                        KeymodeScores[i].Rating = ScoreRating;
                        KeymodeScores[i].ScoreID = NewTopScore.ScoreID;
                        KeymodeScores.Sort((a, b) => b.Rating.CompareTo(a.Rating)); //cba to make two passes again, just gonna sort
                    }
                    //(otherwise do nothing)
                    inserted = true;
                    break; //score has now been handled
                }
                else if (KeymodeScores[i].Rating < ScoreRating) //find a score below this one
                {
                    inserted = true;
                    KeymodeScores.Insert(i, NewTopScore); //insert it here
                    break; //score has now been handled
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
                    Utilities.Logging.Log("Could not retrieve score data from " + score.FileIdentifier + ": " + e.ToString(), Utilities.Logging.LogType.Error);
                    continue;
                }
                yield return new ScoreInfoProvider(s, c);
            }
        }

        public Utilities.TaskManager.UserTask RecalculateTop()
        {
            return (Output) =>
            {
                PhysicalBest = new List<TopScore>[8];
                TechnicalBest = new List<TopScore>[8];
                List<string> oldHashes = new List<string>();
                foreach (string hash in Game.Gameplay.ScoreDatabase.data.Keys)
                {
                    ChartSaveData d = Game.Gameplay.ScoreDatabase.data[hash];
                    if (!ChartLoader.Cache.Charts.ContainsKey(d.Path))
                    {
                        Utilities.Logging.Log("Found score data for " + d.Path + " - the file has been deleted so this will be deleted");
                        oldHashes.Add(hash);
                        continue;
                    }
                    Chart c = ChartLoader.Cache.LoadChart(ChartLoader.Cache.Charts[d.Path]);
                    if (c.GetHash() != hash)
                    {
                        Utilities.Logging.Log("Found old score data for " + d.Path + " - the file is different so this will be deleted");
                        oldHashes.Add(hash);
                        continue;
                    }
                    foreach (Score s in d.Scores)
                    {
                        SetScore(s, c);
                    }
                }
                lock (Game.Gameplay.ScoreDatabase.data) //nothing really interferes but this is just in case
                {
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
