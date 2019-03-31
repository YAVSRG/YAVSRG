using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Gameplay
{
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
}
