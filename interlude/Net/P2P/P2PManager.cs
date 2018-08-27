using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Open.Nat;
using YAVSRG.Net.P2P.Protocol.Packets;

namespace YAVSRG.Net.P2P
{
    public class P2PManager
    {
        public SocketServer Server;
        public SocketClient Client;

        public bool SyncCharts = false;

        public string LobbyKey = "";

        public bool Hosting
        {
            get
            {
                return Server?.Running == true;
            }
        }

        public bool Connected
        {
            get
            {
                return Client?.Closed == false;
            }
        }

        private async void SetupNAT()
        {
            var discoverer = new NatDiscoverer();
            NatDevice device = await discoverer.DiscoverDeviceAsync();
            try
            {
                var ip = await device.GetExternalIPAsync();
                await device.CreatePortMapAsync(new Mapping(Open.Nat.Protocol.Tcp, 32767, 32767, 3600000, "Interlude-Lobby"));
                Utilities.Logging.Log("Port mapping seems to have worked. External IP is " + ip.ToString());
                LobbyKey = Convert.ToBase64String(ip.GetAddressBytes());
            }
            catch (Exception e)
            {
                Utilities.Logging.Log("Port mapping failed: " + e.ToString(), Utilities.Logging.LogType.Error);
            }
        }

        public void HostLobby()
        {
            if (!Hosting && !Connected)
            {
                SetupNAT();
                Server = new SocketServer();
                if (!Server.Start())
                {
                    Utilities.Logging.Log("Couldn't host lobby!", Utilities.Logging.LogType.Warning);
                    Server = null;
                    return;
                }
                JoinLobby(16777343);
            }
        }

        public void CloseLobby()
        {
            if (Hosting)
            {
                Server?.Shutdown();
                Client?.Disconnect();
                LobbyKey = "";
            }
        }

        public void Disconnect()
        {
            if (Hosting)
            {
                CloseLobby();
                return;
            }
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
                JoinLobby(KeyToIP(key));
            }
            catch
            {
                Utilities.Logging.Log("Invalid lobby code: " + key, Utilities.Logging.LogType.Warning);
            }
        }

        public ClientWrapper[] Clients
        {
            get
            {
                return Server?.Clients;
            }
        }

        public void Update()
        {
            Server?.Update();
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

        private long KeyToIP(string key)
        {
            byte[] b = Convert.FromBase64String(key);
            return b[0] + ((long)b[1] << 8) + ((long)b[2] << 16) + ((long)b[3] << 24);
        }

        private string IPToKey(long ip)
        {
            return Convert.ToBase64String(BitConverter.GetBytes(ip));
        }
    }
}
