using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.IO.Pipes;
using System.IO;

namespace YAVSRG.Utilities
{
    class PipeHandler
    {
        static NamedPipeServerStream server;

        public static void Open()
        {
            server = new NamedPipeServerStream("Interlude", PipeDirection.In);
        }

        public static void ReadingThread()
        {
            using (StreamReader r = new StreamReader(server))
            {
                while (true)
                {
                    server.WaitForConnection();
                    string[] l = r.ReadLine().TrimEnd().Split('¬');
                    if (l[0] == "open")
                    {
                        //Game.Screens.AddDialog(new Interface.Dialogs.TextDialog(l[1], (g) => { }));
                        Charts.ChartLoader.ImportArchive(l[1]);
                        Charts.ChartLoader.Refresh();
                    }
                    else if (l[0] == "show")
                    {
                        Game.Instance.ExpandFromIcon();
                    }
                    server.Disconnect();
                }
            }
        }

        public static void Close()
        {
            server.Close();
            server.Dispose();
        }

        public static void SendData(string type, string data)
        {
            NamedPipeClientStream client = new NamedPipeClientStream(".", "Interlude", PipeDirection.Out);
            client.Connect(2000);
            using (StreamWriter w = new StreamWriter(client))
            {
                w.WriteLine(type + "¬" + data);
                client.Flush();
            }
            client.Close();
            client.Dispose();
        }
    }
}
