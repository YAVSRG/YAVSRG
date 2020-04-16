using Prelude.Utilities;
using Prelude.Net;
using Prelude.Net.Protocol.Packets;

namespace Interlude.IO.Net
{
    public class NetManager
    {
        public SocketClient Client;

        public bool Connected
        {
            get
            {
                return (Client?.Closed == false) && (Client?.Connected == true);
            }
        }

        public void Disconnect()
        {
            if (Connected)
            {
                Client?.Disconnect();
            }
        }

        public void Connect()
        {
            if (!Connected)
            {
                Game.Tasks.AddTask((Output) =>
                {
                    Client = new SocketClient(16777343); Client.SendPacket(new PacketAuth()); return Client.Connected;
                }, (t) => Logging.Log(t ? "Connected to remote server" : "Connection failed"), "Connect", false);
            }
        }
        
        public void Update()
        {
            Client?.Update();
        }

        public void SendMessage(string msg)
        {
            Client?.SendPacket(new PacketMessage() { text = msg });
        }

        public void SendPacket(object packet)
        {
            Client?.SendPacket(packet);
        }
    }
}
