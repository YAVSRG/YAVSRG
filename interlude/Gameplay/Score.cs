using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

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
        public float rating;
        public float accuracy;
        public string mods;
        public string hash;
        public string abspath;
    }
}
