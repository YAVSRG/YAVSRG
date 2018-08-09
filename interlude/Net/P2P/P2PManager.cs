using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Open.Nat;

namespace YAVSRG.Net.P2P
{
    public class P2PManager
    {
        SocketServer server;
        SocketClient client;

        public bool Hosting
        {
            get
            {
                return server != null;
            }
        }

        public bool Connected
        {
            get
            {
                return client?.Closed == false;
            }
        }

        private async void SetupNAT()
        {
            var discoverer = new NatDiscoverer();
            NatDevice device = await discoverer.DiscoverDeviceAsync();
            var ip = await device.GetExternalIPAsync();
            await device.CreatePortMapAsync(new Mapping(Open.Nat.Protocol.Tcp, 32767, 32767, 3600000, "Interlude-Lobby"));
            Utilities.Logging.Log("Port mapping seems to have worked. External IP is " + ip.ToString());
        }

        public void HostLobby()
        {
            if (!Hosting && !Connected)
            {
                SetupNAT();
                server = new SocketServer();
                server.Start();
                JoinLobby(16777343);
            }
        }

        public void JoinLobby(long address)
        {
            if (!Connected)
            {
                client = new SocketClient(address);
            }
        }

        public void Update()
        {
            server?.Update();
            client?.Update();
        }
    }
}
