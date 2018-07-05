using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
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

        public List<TopScore>[] Scores = new List<TopScore>[8];

        public void SetScore(Score c)
        {
            if (Scores[c.keycount - 3] == null)
            {
                Scores[c.keycount - 3] = new List<TopScore>();
            }
            List<TopScore> KeymodeScores = Scores[c.keycount - 3];
            ScoreSystem score = ScoreSystem.GetScoreSystem(ScoreType.Default);
            var hd = ScoreTracker.StringToHitData(c.hitdata, c.keycount);
            score.ProcessScore(hd);
            float acc = score.Accuracy();
            float rating = Charts.DifficultyRating.PlayerRating.GetRating(new Charts.DifficultyRating.RatingReport(Game.Gameplay.GetModifiedChart(c.mods), c.rate, c.playstyle), hd);
            TopScore ts = new TopScore() { rating = rating, accuracy = acc, abspath = System.IO.Path.Combine(Game.CurrentChart.Data.SourcePath, Game.CurrentChart.Data.File), hash = Game.CurrentChart.GetHash(), mods = Game.Gameplay.GetModString(c.mods, c.rate, c.playstyle) };
            bool inserted = false;

            for (int i = 0; i < KeymodeScores.Count; i++) //two passes cause im dumb
            {
                if (KeymodeScores[i].abspath == ts.abspath)
                {
                    if (rating < KeymodeScores[i].rating)
                    {
                        inserted = true;
                    }
                    else
                    {
                        KeymodeScores.RemoveAt(i);
                    }
                    break;
                }
            }

            for (int i = 0; i < KeymodeScores.Count; i++)
            {
                if (!inserted && KeymodeScores[i].rating < rating)
                {
                    KeymodeScores.Insert(i, ts);
                    inserted = true;
                    break;
                }
            }
            if (!inserted)
            {
                KeymodeScores.Add(ts);
            }
            if (KeymodeScores.Count > 50)
            {
                KeymodeScores.RemoveAt(50);
            }
        }
    }
}
