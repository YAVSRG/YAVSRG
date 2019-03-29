using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Prelude.Gameplay.Charts.YAVSRG;
using Prelude.Gameplay.Watchers;

namespace Prelude.Gameplay
{
    public class Score
    {
        public DateTime time;
        public string hitdata;
        public string player;
        public string playerUUID;
        public Dictionary<string, string> mods;
        public float rate;
        public DifficultyRating.KeyLayout.Layout layout;
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
}
