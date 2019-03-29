using System;
using System.Net.Sockets;
using Prelude.Utilities;

namespace Interlude.Net.P2P
{
    public class ClientWrapper : SocketWrapper
    {
        private DateTime lastPingSent = DateTime.Now.AddMinutes(-1);
        private DateTime lastPingReceived = DateTime.Now;
        public bool LoggedIn = false;
        public bool ExpectingScore = false;
        public string Username = "";

        public ClientWrapper(Socket s): base(s)
        {

        }

        public override void Update(int id)
        {
            if ((DateTime.Now - lastPingSent).TotalMilliseconds > 10000)
            {
                SendPacket(new Protocol.Packets.PacketPing() { id = id });
                lastPingSent = DateTime.Now;
            }
            else if ((lastPingSent - lastPingReceived).TotalMilliseconds > 15000)
            {
                Logging.Log("Client with id " + id.ToString() + " didn't respond to ping request", "");
                Disconnect();
            }
            base.Update(id);
        }

        public void Ping()
        {
            lastPingReceived = DateTime.Now;
        }

        public void Auth(Protocol.Packets.PacketAuth data)
        {
            if (data.protocolversion != Protocol.Protocol.PROTOCOLVERSION)
            {
                Logging.Log("Client has a version mismatch", "");
                Disconnect();
            }
            else
            {
                Username = data.username;
                LoggedIn = true;
            }
        }
    }
}
