using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace YAVSRG.Net.P2P.Protocol.Packets
{
    public class PacketMessage : Packet<PacketMessage>
    {
        public string text;
    }
}
