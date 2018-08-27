using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Net.Sockets;
using System.Net;
using YAVSRG.Net.P2P.Protocol.Packets;

namespace YAVSRG.Net.P2P
{
    public class SocketClient : SocketWrapper
    {
        public SocketClient(long ip) : base(null)
        {
            try
            {
                sock = new Socket(SocketType.Stream, ProtocolType.Tcp);
                sock.Connect(new IPAddress(ip), 32767);

                SendPacket(new PacketAuth() { protocolversion = Protocol.Protocol.PROTOCOLVERSION, username = Game.Options.Profile.Name });

                PacketPing.OnReceive += HandlePing;
                PacketMessage.OnReceive += HandleMessage;
                PacketPlay.OnReceive += HandlePlay;
                PacketDisconnect.OnReceive += HandleDisconnect;
            }
            catch (Exception e)
            {
                Error(e);
            }
        }

        public void Update()
        {
            base.Update(-1);
            if (Closed)
            {
                PacketPing.OnReceive -= HandlePing;
                PacketMessage.OnReceive -= HandleMessage;
                PacketPlay.OnReceive -= HandlePlay;
                PacketDisconnect.OnReceive -= HandleDisconnect;
                Destroy();
            }
        }

        public override void Disconnect()
        {
            SendPacket(new PacketDisconnect());
            base.Disconnect();
        }

        private void HandlePing(PacketPing packet, int id)
        {
            if (id == -1)
            {
                SendPacket(packet); //ping it back
            }
        }

        private void HandleMessage(PacketMessage packet, int id)
        {
            if (id == -1) //id is -1 when you receive the packet from the server as a client. if you are hosting the id will be 0 or above and should be handled by the server, NOT here
            {
                Game.Screens.Toolbar.Chat.AddLine("Lobby", packet.text);
            }
        }

        private void HandlePlay(PacketPlay packet, int id)
        {
            if (id == -1)
            {
                if (!(Game.Screens.Current is Interface.Screens.ScreenPlay) && Game.Multiplayer.SyncCharts)
                {
                    foreach (string path in Charts.ChartLoader.Cache.Charts.Keys)
                    {
                        var c = Charts.ChartLoader.Cache.Charts[path];
                        if (c.hash == packet.hash)
                        {
                            //needs some sanity checks on it
                            Game.Options.Profile.Rate = packet.rate;
                            Game.Gameplay.SelectedMods = packet.mods;
                            Charts.ChartLoader.SwitchToChart(c, false);
                            Game.Gameplay.PlaySelectedChart();
                            return;
                        }
                    }
                    Utilities.Logging.Log("Couldn't find the chart being playing in the lobby: " + packet.name + " [" + packet.diff + "] from " + packet.pack);
                }
                else
                {
                    SendPacket(new PacketScore());
                }
            }
        }

        private void HandleDisconnect(PacketDisconnect packet, int id)
        {
            if (id == -1)
            {
                Disconnect();
            }
        }
    }
}
