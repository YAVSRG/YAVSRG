using Prelude.Utilities;
using Interlude.Net.P2P.Protocol.Packets;

namespace Interlude.Net.P2P
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
                    Client = new SocketClient(16777343); return Client.Connected;
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
