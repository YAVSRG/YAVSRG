using System;
using System.IO;
using System.Data.SQLite;
using Prelude.Net.Protocol.Packets;

namespace InterludeServer
{
    class Program
    {
        public static SQLiteConnection Database;

        static void Main(string[] args)
        {
            //Database = new SQLiteConnection(@"Data Source=data.db; Version=3");
            //Database.Open();
            var session = Database.CreateSession("Data");
            Directory.CreateDirectory("Scores");
            PacketScore.OnReceive += ScoreTable.HandlePacketScore;
            PacketScoreboard.OnReceive += ScoreTable.HandlePacketScoreboard;
            Prelude.Utilities.Logging.OnLog += (s, d, t) => {
                string r = "[" + t.ToString() + "] " + s + (d != "" ? ": " + d : "");
                Console.WriteLine(s);
            };
            var Server = new SocketServer("Detour");
            if (!Server.Start())
            {
                Console.WriteLine("Could not start server");
            }
            else
            {
                while (true)
                {
                    Server.Update();
                }
            }
            Console.WriteLine("Server shutting down.");
            Server.Shutdown();
            Database.Close();
        }
    }
}
