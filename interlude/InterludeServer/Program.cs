using System;
using System.IO;
using Interlude.Net.P2P;

namespace InterludeServer
{
    class Program
    {
        static void Main(string[] args)
        {
            Directory.CreateDirectory("Scores");
            Interlude.Net.P2P.Protocol.Packets.PacketScore.OnReceive += ScoreTable.HandlePacketScore;
            Interlude.Net.P2P.Protocol.Packets.PacketScoreboard.OnReceive += ScoreTable.HandlePacketScoreboard;
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
        }
    }
}
