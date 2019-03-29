using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Interlude.Net.P2P.Protocol.Packets
{
    public class PacketAuth : Packet<PacketAuth>
    {
        public string username;
        public int protocolversion;
        //some other stuff to verify you are who you are
    }
}
