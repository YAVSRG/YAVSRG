using System;
using System.Net.Sockets;
using System.Net;
using Prelude.Net.Protocol.Packets;

namespace Prelude.Net
{
    //Connects to a SocketServer. Handles basic protocol like pinging and provides an interface to send and receieve packets
    //This is all you need to make your own bot/client
    public class SocketClient : SocketWrapper
    {
        public bool Connected;

        public SocketClient(long ip) : base(null)
        {
            try
            {
                sock = new Socket(SocketType.Stream, ProtocolType.Tcp);
                sock.Connect(new IPAddress(ip), 32767);

                //SendPacket(new PacketAuth() { protocolversion = Protocol.Protocol.PROTOCOLVERSION, username = Game.Options.Profile.Name });

                PacketPing.OnReceive += HandlePing;
                PacketMessage.OnReceive += HandleMessage;
                PacketDisconnect.OnReceive += HandleDisconnect;
                Connected = true;
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
                PacketDisconnect.OnReceive -= HandleDisconnect;
                Connected = false;
                Destroy();
            }
        }

        public override void Disconnect()
        {
            if (Connected)
            {
                Connected = false;
                SendPacket(new PacketDisconnect());
            }
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
            if (id == -1)
            {
                Utilities.Logging.Log(packet.text, "", Utilities.Logging.LogType.Info);
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
