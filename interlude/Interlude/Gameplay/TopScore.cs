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
        public DateTime Timestamp;
        public float Rating;

        public TopScore(string FileID, DateTime Timestamp, float Rating)
        {
            FileIdentifier = FileID;
            this.Timestamp = Timestamp;
            this.Rating = Rating; //only used to compare - is recalculated when displaying
        }
    }
}
