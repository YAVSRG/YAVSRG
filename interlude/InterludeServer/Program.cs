using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Net.P2P;

namespace InterludeServer
{
    class Program
    {
        static void Main(string[] args)
        {
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
