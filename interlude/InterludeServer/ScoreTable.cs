using System;
using System.Collections.Generic;
using Prelude.Gameplay;
using Prelude.Utilities;
using Prelude.Gameplay.DifficultyRating;
using Interlude.Net.P2P.Protocol.Packets;

namespace InterludeServer
{
    class ScoreTable
    {
        public class OnlineScore
        {
            public string Data;
            public DateTime Time;
            public string PlayerUUID;
            public Dictionary<string, DataGroup> Mods;
            public float Rate;
            public KeyLayout.Layout Playstyle;
        }

        public List<OnlineScore> Scores = new List<OnlineScore>();

        public static void HandleScoreUpload(string playeruuid, string charthash, Score score)
        {
            if (charthash != null && score != null) //test for valid score/input
            {
                //todo: cache these files
                var file = GetScoreTable(charthash);
                file.Scores.Add(new OnlineScore() { Data = score.hitdata, Mods = score.selectedMods, Time = score.time, Rate = score.rate, Playstyle = score.layout, PlayerUUID = playeruuid });
                Interlude.Utils.SaveObject(file, "Scores/" + charthash + ".json");
                Console.WriteLine("Saved score");
            }
        }

        public static void GetScoreList(string charthash)
        {
            var file = GetScoreTable(charthash);
            //todo: filters and stuff. for now return them all
            List<Score> output = new List<Score>();
            foreach (OnlineScore s in file.Scores)
            {
                output.Add(new Score() { hitdata = s.Data, player = "No lookup table yet", playerUUID = s.PlayerUUID, layout = s.Playstyle, rate = s.Rate, selectedMods = s.Mods, time = s.Time });
            }
        }

        public static ScoreTable GetScoreTable(string charthash)
        {
            try
            {
                return Interlude.Utils.LoadObject<ScoreTable>("Scores/" + charthash + ".json");
            }
            catch
            {
                return new ScoreTable();
            }
        }

        public static void HandlePacketScore(PacketScore p, int i)
        {
            HandleScoreUpload("__", p.chartHash, p.score);
            Console.WriteLine("Recieved packet");
        }

        public static void HandlePacketScoreboard(PacketScoreboard p, int i)
        {

        }
    }
}
