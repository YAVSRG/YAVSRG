using System;
using System.Net.Sockets;
using Prelude.Utilities;
using Prelude.Net;
using Prelude.Net.Protocol.Packets;

namespace InterludeServer
{
    //Manages a socket connection from elsewhere by handling it logging in and dropping connection if ping response is too long
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
                SendPacket(new PacketPing() { id = id });
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

        //todo: flesh out to proper authentication
        public void Auth(PacketAuth data)
        {
            if (data.protocolversion != Prelude.Net.Protocol.Protocol.PROTOCOLVERSION)
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
