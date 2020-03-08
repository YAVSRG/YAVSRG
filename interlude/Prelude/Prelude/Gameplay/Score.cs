using System;
using System.Collections.Generic;
using Prelude.Utilities;

namespace Prelude.Gameplay
{
    //data structure for scores
    public class Score
    {
        //timestamp of when score was achieved
        public DateTime time;
        
        //full data of ms deviations - it's compressed into a base64 string for ease of transport/saving to file in line with JSON
        public string hitdata;

        //username of the player who got the score
        public string player;

        //uuid of the profile that got that score (so if you change profile name the score can be connected to the same profile still)
        public string playerUUID;

        //dictionary of the mod configuration
        public Dictionary<string, DataGroup> selectedMods = new Dictionary<string, DataGroup>();

        //rate chart was played at
        public float rate;

        //playstyle used to play the chart (used by diffcalc)
        public DifficultyRating.KeyLayout.Layout layout;

        //how many keys in the chart - in future mods could change the keycount of a charts so this allows for quickly matching scores to keymodes
        public int keycount;

        //helper method to assign badge to a score in the style of stepmania
        //todo: support for ComboBreaks counter from scoring
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
}
