using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using YAVSRG.Charts.YAVSRG;

namespace YAVSRG.Gameplay
{
    public class Score
    {
        public DateTime time;
        public string hitdata;
        public string player;
        public Dictionary<string, string> mods;
        public float rate;
        public string playstyle;
        public int keycount;

        public static string GetScoreBadge(int[] judgements)
        {
            int perf = judgements[1];
            int great = judgements[2];
            int cbs = 0;
            for (int i = 3; i < judgements.Length; i++)
            {
                cbs += judgements[i];
            }
            string badge = BadgeLogic(cbs, "MF", "SDCB", "CLEAR");
            badge = badge == "" ? BadgeLogic(great,"BF","SDG","FC") : badge;
            badge = badge == "" ? BadgeLogic(perf, "WF", "SDP", "PFC") : badge;
            badge = badge == "" ? "MFC" : badge;
            return badge;
        }

        static string BadgeLogic(int count, string one, string singledigit, string lots)
        {
            if (count > 0)
            {
                if (count > 1)
                {
                    if (count > 9)
                    {
                        return lots;
                    }
                    return singledigit;
                }
                return one;
            }
            return "";
        }
    }

    public class TopScore
    {
        public string FileIdentifier;
        public int ScoreID;
        public float Rating;

        public TopScore(string FileID, int ScoreID, float Rating)
        {
            FileIdentifier = FileID;
            this.ScoreID = ScoreID;
            this.Rating = Rating; //only used to compare - is recalculated when displaying
        }
    }

    public class ScoreInfoProvider
    {
        Score _score;
        Chart _chart;
        Charts.DifficultyRating.RatingReport _rating;
        string _mods;
        ScoreSystem _scoring;
        ScoreTracker.HitData[] _hitdata;
        float? _physical, _technical;

        public ScoreInfoProvider(Score Score, Chart Chart)
        {
            _score = Score;
            _chart = Chart;
            _hitdata = ScoreTracker.StringToHitData(_score.hitdata, _score.keycount);
        }

        public ChartHeader Data
        {
            get { return _chart.Data; }
        }

        public string Mods
        {
            get { if (_mods == null) { _mods = Game.Gameplay.GetModString(Game.Gameplay.GetModifiedChart(_score.mods, _chart), _score.rate, _score.playstyle); } return _mods; }
        }

        public string Accuracy
        {
            get { if (_scoring == null) { _scoring = ScoreSystem.GetScoreSystem(Game.Options.Profile.ScoreSystem); _scoring.ProcessScore(_hitdata); } return _scoring.FormatAcc(); }
        }

        public int BestCombo
        {
            get { if (_scoring == null) { _scoring = ScoreSystem.GetScoreSystem(Game.Options.Profile.ScoreSystem); _scoring.ProcessScore(_hitdata); } return _scoring.BestCombo; }
        }

        public Charts.DifficultyRating.RatingReport RatingData
        {
            get { if (_rating == null) { _rating = new Charts.DifficultyRating.RatingReport(Game.Gameplay.GetModifiedChart(_score.mods, _chart), _score.rate, _score.playstyle); } return _rating; }
        }

        public float PhysicalPerformance
        {
            get { if (_physical == null) { _physical = Charts.DifficultyRating.PlayerRating.GetRating(RatingData, _hitdata); } return (float)_physical; }
        }

        public float TechnicalPerformance
        {
            get { if (_technical == null) { _technical = 0f; } return (float)_technical; }
        }

        public DateTime Time
        {
            get { return _score.time; }
        }

        public string Player
        {
            get { return _score.player; }
        }
    }
}
