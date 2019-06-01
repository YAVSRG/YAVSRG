using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prelude.Gameplay;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay.ScoreMetrics;
using Prelude.Gameplay.DifficultyRating;

namespace Interlude.Gameplay
{
    public class ScoreInfoProvider
    {
        Score _score;
        Chart _chart;
        RatingReport _rating;
        string _mods;
        ScoreSystem _scoring;
        HitData[] _hitdata;
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

        public HitData[] HitData
        {
            get { return _hitdata; }
        }

        public string Mods
        {
            get { if (_mods == null) { _mods = Game.Gameplay.GetModString(Game.Gameplay.GetModifiedChart(_score.selectedMods, _chart), _score.rate, _score.layout); } return _mods; }
        }

        public ScoreSystem ScoreSystem
        {
            get
            {
                if (_scoring == null) { _scoring = Game.Options.Profile.GetScoreSystem(Game.Options.Profile.SelectedScoreSystem); _scoring.ProcessScore(HitData); }
                return _scoring;
            }
        }

        public string FormattedAccuracy
        {
            get { return ScoreSystem.FormatAcc(); }
        }

        public string ScoreShorthand
        {
            get { return ScoreSystem.ComboBreaks.ToString()+" / "+Utils.RoundNumber(ScoreSystem.Accuracy())+"%"; }
        }

        public int BestCombo
        {
            get { return ScoreSystem.BestCombo; }
        }

        public RatingReport RatingData
        {
            get { if (_rating == null) { _rating = new RatingReport(Game.Gameplay.GetModifiedChart(_score.selectedMods, _chart), _score.rate, _score.layout); } return _rating; }
        }

        public float PhysicalPerformance
        {
            get { if (_physical == null) { _physical = PlayerRating.GetRating(RatingData, HitData); } return (float)_physical; }
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
