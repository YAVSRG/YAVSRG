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
                Destroy();
            }
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
    }
}
