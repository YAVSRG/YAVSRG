using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Interlude.Utilities;
using Interlude.Net.P2P.Protocol.Packets;

namespace Interlude.Net.P2P
{
    public class P2PManager
    {
        public SocketClient Client;

        public string LobbyKey = "";

        public bool Connected
        {
            get
            {
                return Client?.Closed == false;
            }
        }

        /*
        private async void SetupNAT()
        {
            var discoverer = new NatDiscoverer();
            try
            {
                NatDevice device = await discoverer.DiscoverDeviceAsync();
                var ip = await device.GetExternalIPAsync();
                await device.CreatePortMapAsync(new Mapping(Open.Nat.Protocol.Tcp, 32767, 32767, 3600000, "Interlude-Lobby"));
                Logging.Log("Port mapping seems to have worked. External IP is " + ip.ToString(), "");
                LobbyKey = Convert.ToBase64String(ip.GetAddressBytes());
            }
            catch (Exception e)
            {
                Logging.Log("Port mapping failed", e.ToString(), Logging.LogType.Error);
            }
        }*/

        public void Disconnect()
        {
            if (Connected)
            {
                Client?.Disconnect();
            }
        }

        private void JoinLobby(long address)
        {
            if (!Connected)
            {
                Client = new SocketClient(address);
            }
        }

        public void JoinLobby(string key)
        {
            try
            {
                JoinLobby(16777343);
            }
            catch
            {
                Logging.Log("Invalid lobby code: " + key, "", Logging.LogType.Warning);
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

        /*
        private long KeyToIP(string key)
        {
            byte[] b = Convert.FromBase64String(key);
            return b[0] + ((long)b[1] << 8) + ((long)b[2] << 16) + ((long)b[3] << 24);
        }

        private string IPToKey(long ip)
        {
            return Convert.ToBase64String(BitConverter.GetBytes(ip));
        }*/
    }
}
